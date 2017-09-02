{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, OverloadedStrings, RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}

module Bot where
    
import qualified Crypto.Saltine.Class as Salt
import Crypto.Saltine.Core.SecretBox (Key, Nonce, secretbox)
import Data.Aeson as Aeson hiding (Result)
import Data.Vector (Vector, find, elem)
import Data.Text (Text,unpack,drop,isPrefixOf,isInfixOf,append)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson.Types hiding (Options,Result)
import Data.ByteString.Lazy (ByteString)
import Data.Either.Combinators
import qualified Data.ByteString.Lazy as BS
import qualified Data.Binary as Bin
import Data.Bits
import Data.Maybe (isJust, fromJust)
import Data.Word
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Char8 as BC8 (unpack, pack)
import Control.Lens hiding ((.=))
import Network.Socket as NS hiding (send, recv)
import Network.Socket.ByteString
import Control.Concurrent
import System.Posix.Time
import Foreign.C.Types
import Network.WebSockets hiding (Response, send)
import Network.WebSockets.Connection (sendCloseCode)
import Control.Monad
import Control.Exception (catch, throwIO)
import System.IO (openFile, openBinaryFile, IOMode(..), hClose, hGetContents, hSetBuffering, BufferMode(..))
import System.IO.Error
import GHC.IO.Exception
import Network.HTTP.Client hiding (responseBody,responseStatus)
import System.Mem (performGC)
import Wuss
import Control.Monad.State
import Control.Monad.Except
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Network.Wreq hiding (Payload,get,put,delete,asValue)
import Prelude hiding (lookup)
import Text.Read (readMaybe)
import qualified Data.List as List (isInfixOf)

-- Standard start of all urls when accessing discord API
stdUrl = "https://discordapp.com/api/v6"

----------------------------------Payload framework--------------------------------------------
{- REPRESENTATION CONVENTION:
    Represents a set of data received/sent through a connection to gateway.discord.gg
    Payload tagged with constructor..
    .. Dispatch represents an event (including event name/type) received from the gateway
    .. Heartbeat represents a heartbeat sequence (keep-alive packet) sent to the gateway
    .. Identify represents an identification object sent to the gateway to authorize yourself
    .. Status is sent to the gateway to change the status of the bot
    .. VoiceStateUpdate is sent to the gateway in order to connect to a voice channel and start transmitting voice.
    .. Resume represents an identification object sent to the gateway to authorize yourself, AND resume a previously disconnected session, if able.
    .. Reconnect represents a request from the gateway for the client to reconnect.
    .. InvalidSession is sent by the gateway in response to a Resume payload, in case the desired session to resume is no longer valid.
    .. Hello represents a greeting from the gateway, containing interval at which to send heartbeats
    .. HeartbeatAck is sent by the gateway in response to a Heartbeat payload, indicating it has acknowledged the Heartbeat.
       Failure to send HeartbeatAck indicates that the client has been disconnected.
    .. Other represents payload for which there is no use here (currently)
   REPRESENTATION INVARIANT:
    eventData in Dispatch is a JSON object received from gateway.discord.gg
    interval > 0
    sq > 0
-}
data Payload = -- Type of received data is determined through an OP-code received with the data
      Dispatch { -- OP 0
        eventData :: Value , -- Internal object.
        sq :: Int ,
        eventName :: String
        }
    | Heartbeat { -- OP 1
        sq :: Int -- Sequence. This is somewhat misleading; heartbeat actually uses the same key ("d") that Dispatch and Identify 
                  -- uses to send event data. This is in contrast to Dispatch, who uses the key "s" to send the sequence.
        }
    | Identify { -- OP 2
        eventData :: Value -- Internal object.
        }
    | Status String -- OP 3
    | VoiceStateUpdate { -- OP 4
        guild_id    :: String
    ,   channel_id  :: Maybe String
        }
    | Resume String String Int -- OP 6
    | Reconnect -- OP 7
    | InvalidSession -- OP 9
    | Hello { -- OP 10
        interval :: Int -- eventData is skipped in favor of plucking out the only internal value of concern; heartbeat_interval
        }
    | HeartbeatAck -- OP 11
    | Other -- Unknown OP (the rest of them).
    deriving (Show)

{-
    Definition of converting JSON -> Payload
    Exctracts data from a JSON of the form defined in the docs of the discord API and
    then stores in under appropiate constructor in Payload (depending on OP-code in JSON)
-}
instance FromJSON Payload where
    parseJSON = withObject "Payload" $ \obj -> 
        (obj .: "op" :: Parser Int) >>= \op ->
            case op of -- determine payload and construct it
                0 ->
                    Dispatch <$> 
                        obj .: "d" <*>  -- Will become eventData
                        obj .: "s" <*>  -- Will become sq
                        obj .: "t"      -- Will become eventName
                1 ->
                    Heartbeat <$>
                        obj .: "d" -- Will become sq
                2 ->
                    Identify <$>
                        obj .: "d" -- Will become eventData
                7 ->
                    pure Reconnect
                9 ->
                    pure InvalidSession
                10 ->
                    Hello <$> 
                        ((obj .: "d") >>= (.: "heartbeat_interval")) -- Will become interval
                11 ->
                    pure HeartbeatAck
                _ ->
                    pure Other

-- Definition of converting Payload -> JSON
-- Constructs JSON as they are defined in docs of the discord API
instance ToJSON Payload where
    toJSON Heartbeat{..} = object $
        [
            "op" .= (1 :: Int) ,
            "d" .= sq
        ]
    toJSON Identify{..} = object $
        [
            "op" .= (2 :: Int) ,
            "d" .= eventData
        ]
    toJSON (Status game) = object $
        [
            "op" .= (3 :: Int) ,
            "d" .= object [
                "since"     .= Null ,
                "game"      .= actual ,
                "status"    .= String "online" ,
                "afk"       .= False
                ]
        ]
        where
            actual :: Value
            actual | null game = Null
                   | otherwise = object ["name" .= game]
    toJSON VoiceStateUpdate{..} = object
        [
            "op" .= (4 :: Int)
        ,   "d" .= object [
                    "guild_id"      .= guild_id
                ,   "channel_id"    .= maybe Null toJSON channel_id
                ,   "self_mute"     .= False
                ,   "self_deaf"     .= False
                ]
        ]
    toJSON (Resume token session_id sq) = object $
        [
            "op" .= (6 :: Int) ,
            "d" .= object [
                    "token" .= token ,
                    "session_id" .= session_id ,
                    "seq" .= sq
                ]
        ]
    toJSON _ = error "Tried to construct payload of a type the bot should never send."


data VoicePayload =
        VoiceIdentify {         -- OP 0
            v_server_id     :: String
        ,   v_user_id       :: String
        ,   v_session_id    :: String
        ,   v_token         :: String
        }
    |   SelectProtocol {        -- OP 1
            v_address       :: Text
        ,   v_UDPport       :: Word16
        }
    |   VoiceReady {            -- OP 2
            v_ssrc          :: Word32
        ,   v_port          :: String
        ,   v_interval      :: Int
        }
    |   VoiceHeartbeat Int      -- OP 3
    |   SessionDescription {    -- OP 4.
            secret_key      :: Key
            }
    |   Speaking Bool           -- OP 5
    |   VoiceOther              -- Among other things, the mystery payload, OP 8

instance FromJSON VoicePayload where
    parseJSON = withObject "VoicePayload" $ \obj -> 
        (obj .: "op" :: Parser Int) >>= \op ->
            case op of -- determine payload and construct it
                0 -> (obj .: "d") >>= \d ->
                    VoiceIdentify <$> 
                            d   .: "server_id"
                        <*> d   .: "user_id"
                        <*> d   .: "session_id"
                        <*> d   .: "token"
                1 -> (obj .: "d") >>= (.: "data") >>= \da ->
                    SelectProtocol <$>
                            da  .: "address"
                        <*> da  .: "port"
                2 -> (obj .: "d") >>= \d ->
                    VoiceReady <$>
                            d   .: "ssrc"
                        <*> (show <$> (d   .: "port" :: Parser Int))
                        <*> d   .: "heartbeat_interval"
                3 ->
                    VoiceHeartbeat <$> obj .: "d"
                4 -> (obj .: "d") >>= (.: "secret_key") >>= \key -> 
                    pure $ SessionDescription $ 
                        maybe (error "Transforming secret_key provided by SessionDescription to Key failed.")
                        id (Salt.decode . Strict.pack $ key)
                5 ->
                    Speaking <$>
                        ((obj .: "d") >>= (.: "speaking"))
                _ -> pure VoiceOther

instance ToJSON VoicePayload where
    toJSON VoiceIdentify{..} = object
        [
            "op"    .= (0 :: Int)
        ,   "d"     .= object
            [
                "server_id"     .= v_server_id
            ,   "user_id"       .= v_user_id
            ,   "session_id"    .= v_session_id
            ,   "token"         .= v_token   
            ]
        ]
    toJSON SelectProtocol{..} = object
        [
            "op"    .= (1 :: Int)
        ,   "d"     .= object
            [
                "protocol"  .= String "udp"
            ,   "data"      .= object
                [
                    "adress"    .= v_address
                ,   "port"      .= v_UDPport
                ,   "mode"      .= String "xsalsa20_poly1305"
                ]
            ]
        ]
    toJSON (VoiceHeartbeat timestamp) = object
        [
            "op"        .= (3 :: Int)
        ,   "d"         .= timestamp
        ]
    toJSON (Speaking speaking) = object
        [
            "op"    .= (5 :: Int)
        ,   "d"     .= object
                [
                    "speaking"  .= speaking
                ,   "delay"     .= (0 :: Int)
                ]
        ]
    toJSON _ = error "Tried to construct voice payload of a type the bot should never send."



----------------------------------Behaviour framework------------------------------------------

{-
    The state of the bot, preserved as it runs.
    "internal" is the internal state, whose type is customizable; as long as its an instance of the BotStateInstance class (which requires it to be readable and showable). Its also the only part of the state that is stored between sessions.
    authHeader is preserved here, so that it doesn't need to be "carried" through each bloody action function.
    the rateLimit hashmaps are to keep track of ratelimits.
    rateLimitGlobal == (Just x) indicates that the bot is globally rate limited. The x is the reset point, in epoch time.
    for rateLimitGuilds and rateLimitChannels, the key (String) is the guild id / channel id respectively.
    In each tuple (Bool, Int), the Bool indicates if the bot is rate limited, and the Int is the reset point, in epoch time.
-}

data BotState a = BotState
    {
        rateLimitGlobal     :: Maybe Int
    ,   rateLimitGuilds     :: HashMap String (Bool, Int)
    ,   rateLimitChannels   :: HashMap String (Bool, Int)
    ,   authHeader          :: Options
    ,   bot_id              :: String
    ,   internal            :: a -- This is the only part that will be saved.
    }
    deriving (Show)


-- The initial state that the bot creates, with provided internal state, authHeader, and bot_id.

initstate :: a -> Options -> String -> BotState a
initstate int aH bid = BotState{rateLimitGlobal = Nothing, rateLimitGuilds = HM.empty, rateLimitChannels = HM.empty, authHeader = aH, bot_id = bid, internal = int}


{- ActionMonad is an alias for the State monad that all actions should now make use of.
    State monads are... weird and complicated. It took some time for me to figure out, and its kind of hard to explain.
    The gist of it is that a State monad doesn't actually wrap a value, like Maybe or Either, or even IO does.
    It instead wraps a function, that, when run with a state as an argument, produces a new state, together with a return value.
    This behind-the-scenes stuff are practically invisible to the developer, because the guys who made the state monad were very clever.
    It even seems like it makes use of side-effects, as you can create an ActionMonad functions that changes the state, and yet returns nothing.
    This is only virtual side effects though; in reality, the state is "carried" as part of the return value.

    The only exception is when you want to retrieve a value from a state monad, outside of that state monad. In that case, you will need to use runStateT,
    that runs the function that the state monad preserves, and returns both the value and the new state, wrapped in some monad;
    in ActionMonad's case, that monad will be IO. It has to be, in order to be able to perform IO actions inside of the ActionMonad.

    Ok, so it SOUNDS complicated, and it really is behind-the-scenes, but it really isn't when it comes to developing with it. 

    ************************ IT'S GOTTEN EVEN WEIRDER NOW! ********************
    I've added an exception handling monad to the stack, too, and wrapped it in newtype,
    which allows me to do cool stuff like using GeneralizedNewtypeDeriving to derive a shitton of stuff.
    The ActionMonad is no longer what is *returned* from Actions and Reactions; that remains StateT (BotState a) IO [Command a]
    I've done this because there should not be any way for an exception to reach the main bot function. This way,
    type safety guarantees that if you use the ActionMonad, you need to catch any and all exceptions.
    To convert an ActionMonad to StateT, use runAM or skipError.

    TO CONSIDER: Also adding readerT to the stack for authHeader and bot_id rather than having these be part of state.



-}
newtype ActionMonad e st a = AM { unAM :: ExceptT e (StateT (BotState st) IO) a } deriving (MonadState (BotState st), MonadError e, MonadIO, Functor, Applicative, Monad)


-- runAM is a way to make sure that there is no way in hell an exception slips by.
runAM :: ActionMonad e s a -> (e -> a) -> StateT (BotState s) IO a
runAM q h = fmap (either h id) (runExceptT (unAM q))

-- skipError is the simplest way to convert a (Re)Action' to Action. If an unhandled exception is raised, skipError will simply return an empty list. 
skipError :: ActionMonad e s [Command a] -> StateT (BotState s) IO [Command a]
skipError q = runAM q (\_ -> [])

-- The BotStateInstance class. All internal botstates must be instances of this class.
class (Show st, Read st) => BotStateInstance st where
    -- The empty internal state
    empty :: st

-- Updates the internal state according to a function.
update :: (a -> a) -> ActionMonad e a ()
update func = modify $ \st@BotState{internal} -> st{internal=(func internal)}

-- Get the internal state.
getInternal :: ActionMonad e a a
getInternal = fmap internal get

-- Gets the internal state, transformed to some value according to the provided function.
-- May be used to get specific elements of an internal state; however, it's probably better to use BotStateInstanceSearchable functions rather than getsInternal.
getsInternal :: (a -> b) -> ActionMonad e a b
getsInternal f = fmap f getInternal

-- Puts an internal state.
putInternal :: a -> ActionMonad e a ()
putInternal a = modify $ \st -> st{internal=a}

-- Optional BotStateInstance class, that defines a function that deletes information within the state.
class BotStateInstance st => BotStateInstanceDeletable st key where
    {-
        The "pure" (read: non-monadic) portion of delete is the pure function, that, given a key and a state, should delete the relevant value bound to that key in that state.
        All instances must define deleter.
    -}
    deleter :: key -> st -> st

-- Deletes a value bound to a key in the state carried by the ActionMonad.
delete :: BotStateInstanceDeletable st key => key -> ActionMonad e st ()
delete = update . deleter


{-
    Optional BotStateInstance class, that defines fetch and insert functions to interact with the state.
    error should be the error type of the ActionMonad.
-}
class BotStateInstanceDeletable st key => BotStateInstanceSearchable error st key value | key -> error where
    -- The pure portion of fetch. Fetches the value within a state bound to the key. Supports potential errors.
    fetcher :: key -> st -> Either error value

    -- The pure portion of insert. Inserts a value bound to a key within a state. Supports potential errors.
    inserter :: key -> value -> st -> Either error st

-- fetches a value bound to a key within the state carried by the ActionMonad. Supports potential errors
-- This feels like a massive hack. I don't like it. Probably no better way.
fetch :: BotStateInstanceSearchable error st key value => key -> ActionMonad error st value
fetch = (>>= eitherToError) . getsInternal . fetcher

-- inserts a key bound to a value within the state carried by the ActionMonad. THIS SHOULD REPLACE ALREADY EXISTING KEYS!
-- Also feels like a hack, but less so.
insert :: BotStateInstanceSearchable error st key value => key -> value -> ActionMonad error st ()
insert k v = do
    eitherstate <- getsInternal (inserter k v)
    newstate    <- eitherToError eitherstate
    putInternal newstate

{-
    the Bool indicates whether the function is performed retroactively (as part of replaying events).

    A normal Action is not actually performed in the ActionMonad, as you need to guarantee that no exceptions are raised once the action completes.
    That's why skipError exists, and is used so frequently (in the provided test behaviour), to convert Action' e a to Action a.

    Command is an instruction for the bot that can't be executed within the very ActionMonad, like shutting down the bot. 

-}
type Action a = Bool -> StateT (BotState a) IO [Command a]
type Action' e a = Bool -> ActionMonad e a [Command a]


{-
    An action that is a reaction to an event.
    In addition to what an Action does, a reaction also takes information about the event it reacts to.
-}
type Reaction a = Value -> Action a
type Reaction' e a = Value -> Action' e a


{-
    The Behaviour data type has now two different constructors; one for all reactions, one for everything else.
    "everything else" keeps track of the name of the bot, its reactions, onTimer, timer, and also the default state for the behaviour.
    defaultState must be an instance of the BotStateInstance class, in order to store it. The name of the Behaviour will be used when saving/loading internal state; the file it loads has the same name.
-}
data Behaviour a =
    Behaviour
        {
            name            :: String
        ,   reactions       :: ReactionSet a
        ,   initAction      :: Action a
        ,   onTimer         :: Action a         --onTimer is obsolete considering actions (including initAction) can use the Commands Fork, Wait and Enqueue to implement the same behaviour, but with better control. Will be removed in a future version.
        ,   timer           :: Int
        ,   defaultState    :: a -- Will become internal state.
        }

data ReactionSet a =
    ReactionSet
        {
            ready                   :: Reaction a
        ,   resumed                 :: Reaction a
        ,   channelCreate           :: Reaction a
        ,   channelUpdate           :: Reaction a
        ,   channelDelete           :: Reaction a
        ,   guildCreate             :: Reaction a
        ,   guildUpdate             :: Reaction a
        ,   guildDelete             :: Reaction a
        ,   guildBanAdd             :: Reaction a
        ,   guildBanRemove          :: Reaction a
        ,   guildEmojisUpdate       :: Reaction a
        ,   guildIntegrationsUpdate :: Reaction a
        ,   guildMemberAdd          :: Reaction a
        ,   guildMemberRemove       :: Reaction a
        ,   guildMemberUpdate       :: Reaction a
        ,   guildMembersChunk       :: Reaction a
        ,   guildRoleCreate         :: Reaction a
        ,   guildRoleUpdate         :: Reaction a
        ,   guildRoleDelete         :: Reaction a
        ,   messageCreate           :: Reaction a
        ,   messageUpdate           :: Reaction a
        ,   messageDelete           :: Reaction a
        ,   messageDeleteBulk       :: Reaction a
        ,   presenceUpdate          :: Reaction a
        ,   gameObject              :: Reaction a
        ,   typingStart             :: Reaction a
        ,   userSettingsUpdate      :: Reaction a
        ,   userUpdate              :: Reaction a
        ,   voiceStateUpdate        :: Reaction a
        ,   voiceServerUpdate       :: Reaction a
        }

-- Non-acting actions simply don't change the state carried into it, and return []. 
defaultBehaviour :: BotStateInstance a => Behaviour a
defaultBehaviour = Behaviour {
        name            = "TrashBot"
    ,   reactions       = defaultReactions
    ,   initAction      = (\_ -> return [])
    ,   onTimer         = (\_ -> return [])
    ,   timer           = 0 -- By default, onTimer will never be called.
    ,   defaultState    = empty
    }

defaultReactions :: ReactionSet a
defaultReactions = ReactionSet {
    ready                   = null,
    resumed                 = null,
    channelCreate           = null,
    channelUpdate           = null,
    channelDelete           = null,
    guildCreate             = null,
    guildUpdate             = null,
    guildDelete             = null,
    guildBanAdd             = null,
    guildBanRemove          = null,
    guildEmojisUpdate       = null,
    guildIntegrationsUpdate = null,
    guildMemberAdd          = null,
    guildMemberRemove       = null,
    guildMemberUpdate       = null,
    guildMembersChunk       = null,
    guildRoleCreate         = null,
    guildRoleUpdate         = null,
    guildRoleDelete         = null,
    messageCreate           = null,
    messageUpdate           = null,
    messageDelete           = null,
    messageDeleteBulk       = null,
    presenceUpdate          = null,
    gameObject              = null,
    typingStart             = null,
    userSettingsUpdate      = null,
    userUpdate              = null,
    voiceStateUpdate        = null,
    voiceServerUpdate       = null
    }
    -- Default non-reacting reaction
    where null = (\_ _ -> return [])




---------------------------------------JSON utilities-------------------------------------------
{- extractJSON json
    PRE: json may be converted to the desired type.
         (Type annotation is needed to indicate desired type.)
    POST: a, where a is json converted to the desired type.
    EXAMPLES:
        (extractJSON (Bool True) :: Bool) == True
        (extractJSON (Number 1.0) :: Int) == 1
        (extractJSON (Number 1.0) :: Double) == 1.0
        (extractJSON (Bool True) :: State) == Bool True
-}
extractJSON :: FromJSON a => Value -> a
extractJSON v = case fromJSON v of
    (Success a) -> a
    (Error a) -> error $ "Bot.extractJSON: Type annotation missing or is faulty: " ++ a


{- filterJSON JSONobject key
   PURPOSE: Extract value under key in JSONobject
   PRE: key is in JSONobject
   POST: value under key in JSONobject
   SIDE-EFFECTS: Raises an error if key is not in JSONobject
-}
filterJSON :: FromJSON a => Value -> Text -> a
filterJSON st key = maybe (error $ "Bot.filterJSON: Key doesn't exist: " ++ show key ++ ". If it exists, you've type annotated incorrectly. Object is: " ++ show st)
    id $ lookupJSON st key

lookupJSON :: FromJSON a => Value -> Text -> Maybe a
lookupJSON (Object object) key = parseMaybe (.: key) object
lookupJSON o _ = error ("Bot.lookupJSON: Not an object: " ++ (show o))  -- Should use type Object instead



-- convertResponse wrapped the result into a monad for... no reason, so I've changed it to return a pure value instead.
convertResponse :: FromJSON v => Response ByteString -> v
convertResponse resp =
    let bs = eitherDecode $ resp ^. responseBody
    in
        either (\err -> error $ "convertResponse: Type annotation missing or is faulty: " ++ show err)
        id bs

----------------------------------Connection interaction----------------------------------------

{- getJSON connection
   PURPOSE: Receive and decode fromJSON objects
   PRE: connection is open
   POST: received payload
   SIDE-EFFECTS: Receives JSON encoded payload through connection
-}
getJSON :: FromJSON a => Connection -> IO a
getJSON conn = fmap (fromJust . Aeson.decode) (receiveData conn)

{- sendJSON connection payload
   PURPOSE: Encode to JSON and send payload through connection
   PRE: connection is open
   POST: ()
   SIDE-EFFECTS: Sends the JSON encoded payload through connection
-}
sendJSON :: ToJSON a => Connection -> a -> IO ()
sendJSON conn = (sendTextData conn) . Aeson.encode


------------------------------------API interaction---------------------------------------------

-- Gives the time as the computer believes it is in epoch time.
epoch :: IO Int
epoch = fmap (\(CTime i) -> fromIntegral i) epochTime

{-
    memoize is a function that checks if a value is bound to the key given within the internal state. If it is, that value is returned.
    If it isn't, then the request is performed, and whatever that returns is inserted within the internal state with the given key, and then returned.
    memoize is extremely useful for avoiding excessive GET requests to obtain non-volatile information.
    If you want some specific part of the response info that is volatile,
    but you want to store the entire response as other parts of it aren't volatile and may come in handy later,
    use memoize' instead.
-}
memoize :: (BotStateInstanceSearchable error state key value) => key -> ActionMonad error state value -> ActionMonad error state value
memoize key question = catchError (fetch key) (\_ -> memoize' key question)

-- memoize' is memoize; however, it ALWAYS performs the request, and updates internal state (overwriting existing values for the key.)
memoize' :: (BotStateInstanceSearchable error state key value) => key -> ActionMonad error state value -> ActionMonad error state value
memoize' key question = do
    answer <- question
    preserveInternalState $ insert key answer
    return answer

-- This class is for custom error types that can "encapsulate" exceptions of type BadRequest. The error type for an ActionMonad needs to be of this class in order to be able to use customRequest in that monad.
class Show e => SupportsNetworkError e where
    fromBR  :: BadRequest -> e
    toBR    :: e -> Maybe BadRequest

instance SupportsNetworkError BadRequest where
    fromBR  = id
    toBR    = Just

-- BadRequest is the exception data type for requests.
data BadRequest = RateLimited Bool Int | BR400 | OtherResponse Int (Response ()) deriving (Show)

-- Parameter is used for customRequst; it indicates the major parameter for use when storing rate limits.
data Parameter = Channel String | Guild String | None
{-
    Command is an instruction for the bot that can't be executed within the very ActionMonad; like shutting down the bot.
-}
data Command a
  = SendPayload Payload                     -- The bot sends this payload to the gateway.
  | Refer       (Payload -> Action a)       -- The next payload that the bot receives is fed directly to this action, that will be performed as soon as that payload is received.
  | Enqueue     (Action a)                  -- Enqueues an action. Not very useful without Fork.
  | FakePayload Payload                     -- The bot treats this payload as though it was received from the Gateway; however, it won't refer it to any action.
  | Voice       String String FilePath      -- Tries to connect to a channel (second string) in a guild (first string) and play an preencoded .dca audio file at FilePath.     
  | Wait        Int                         -- Forces the bot to wait i microseconds before processing next command. Will block other payloads from being consumed unless Fork is used.
  | Fork        [Command a]                 -- Executes the listed commands in a new thread.
  | ShutDown                                -- Shuts the bot down.

{-
    wait performs a request, but if that request returns an RateLimited exception, then wait until the rateLimits reset, then perform the request again.
    Repeat until either the request is performed without error, or with a non-RateLimited error.

    if you're wondering what liftIO is, it's needed to perform IO in the ActionMonad.
    NO, liftIO is not unsafePerformIO and is, in fact, completely safe.
    See, the ActionMonad uses IO internally, but to make IO functions type correct so they may be executed from the ActionMonad, liftIO is neccessary.
-}
wait :: SupportsNetworkError e => ActionMonad e s a -> ActionMonad e s a
wait q = catchError q $ \result ->
    case (toBR result) of
        Just (RateLimited _ x) -> liftIO (threadDelay ((x + 1) * 1000000)) >> (wait q)
        _   -> throwError result
{-
    Performs a request, but in case of failure, defaults to Monoidic empty.
    Now generalized to all MonadErrors. Makes it a lot more dangerous for the same reasons try is.
-}
mtry :: (MonadError e m, Monoid v) => m v -> m v
mtry = flip catchError (\_ -> return mempty)

{-
    Performs a request, completely ignoring what it returns, or if it even succeeds.
    With the changes to the ActionMonad, try has become a very devious function, and you should be careful of what you pass to it.
    It's like void, however, once it meets an exception, it won't 'report' it, and let the rest of the program outside try go on as normal.
    *It wont even revert state to how it was before try was called*. Basically, only use try if you're certain what you pass to it cannot fuck up IO or State-wise.
    Generalized to all MonadErrors.
-}
try :: MonadError e m => m v -> m ()
try = mtry . void

-- catchErrorWithState is catchError, however, the handler function is also provided with the the monad's state before the function was called.
catchErrorWithState :: (MonadError e m, MonadState s m) => m a -> (s -> e -> m a) -> m a
catchErrorWithState q h = do
    oldst <- get
    catchError q (h oldst)


-- In case m a produces an exception, preserveState will revert the state to what it was before it was called.
-- This is not always desirable; for example, rate limits produces an exception. Use preserveInternalState if internal state is the only thing you want to preserve.
preserveState :: (MonadError e m, MonadState s m) => m a -> m a
preserveState = flip catchErrorWithState (\st e -> put st >> throwError e)

preserveInternalState :: ActionMonad e s a -> ActionMonad e s a
preserveInternalState = flip catchErrorWithState (\BotState{internal} e -> putInternal internal >> throwError e)

-- asValue is a simple alternative to type annotating. However, type inference typically does the job without needing either type annotating or asValue.
-- asValue has become more neccesary now ignore has been replaced by void
asValue :: ActionMonad e s Value -> ActionMonad e s Value
asValue = id

-- asRequest is another alternative to type annotating, when not even the error type matters.
asRequest :: ActionMonad BadRequest s Value -> ActionMonad BadRequest s Value
asRequest = id

{- The big baddie itself; customRequest.
    customRequest is the function that behaviour developers should use for ALL HTTP requests.
    It keeps tracks of rate limits, and returns a host of different exceptions in case of failure.
    The URL path the request will be on is:
        (stdUrl ++ (if param /= None, then empty string. Otherwise: ("/" ++ (whatever string is wrapped in param))) ++ path)
    if payload is Nothing, the request will execute with no payload. Otherwise, it will execute with the payload x, where payload == (Just x).
-}
customRequest :: (SupportsNetworkError e, FromJSON v) => Parameter -> String -> String -> Maybe Value -> ActionMonad e s v
customRequest param path method payload = do
    time <- liftIO epoch                            -- Get current time
    st@BotState{rateLimitGlobal, authHeader} <- get -- Gets state's opinion about global rate limit. Also extract authHeader here so its in scope for auxiliary functions.
    case rateLimitGlobal of
        Just x  ->                                  -- We've been globally rate limited in the past. Check if we're past the reset point
            let diff = (x - time)
            in if diff >= 0                         -- To be on the safe side, we assume to be globally rate limited even when on the reset point. This is probably unnecessary.
                then liftIO (putStrLn "Globally rate limited. (According to State)") >> (throwError . fromBR) (RateLimited True diff)
                else put st{rateLimitGlobal=Nothing} >> continue authHeader time    -- If we're not globally rate limited anymore, set rateLimitGlobal to Nothing.
        _       -> continue authHeader time
    where
        continue :: (SupportsNetworkError e, FromJSON v) => Options -> Int -> ActionMonad e s v
        continue authHeader time = do
            case param of       -- Determines what HashMap we read from, and how to construct the URL address.
                (Guild s)   -> do
                    BotState{rateLimitGuilds} <- get
                    continue' s rateLimitGuilds "/guilds/"
                (Channel s) -> do
                    BotState{rateLimitChannels} <- get
                    continue' s rateLimitChannels "/channels/"
                None        -> continueNoParam  -- The request has no major parameters, meaning it may only be globally rate limited.

            where
                continueNoParam :: (SupportsNetworkError e, FromJSON v) => ActionMonad e s v
                continueNoParam = do
                    result <- liftIO $ catch performRequest handler -- Performs the request. We catch the exceptions that wreq's functions throw, so that we may instead wrap in in the Either monad.

                    case result of
                        (Right resp)    -> return $ convertResponse resp            -- The request went through fine. Converts the body to the desired FromJSON value using convertResponse.
                        (Left resp)     -> liftIO epoch >>= \time ->                -- An exception was thrown. We recheck the time. TODO: Rather than recheck time, use timestamp header
                            case resp ^. responseStatus ^. statusCode of
                                400 -> throwError . fromBR $ BR400                                          -- The request was bad for some strange reason. Likely programmer error, but may also be incorrect arguments to a [Command].
                                429 | (resp ^? responseHeader "X-RateLimit-Global") == Just "true" -> do    -- We've run into a global rate limit (we shouldn't be able to run into a local one, as we have no major parameters in our URL path.)
                                    liftIO $ putStrLn "Globally rate limited. (429 response)"
                                    let retryafter = (read . BC8.unpack $ resp ^. responseHeader "Retry-After") :: Int  -- Determine the reset point.
                                    get >>= \st -> put st{rateLimitGlobal=(Just(time + retryafter))}                    -- Update state to indicate we're globally rate limited.
                                    throwError . fromBR $ RateLimited True retryafter                                   -- Return an exception.
                                x -> do
                                    throwError . fromBR $ OtherResponse x resp                                          -- Was some other response. We return statusCode and the response itself.
                    where
                        performRequest :: IO (Either (Response ()) (Response ByteString))
                        performRequest = return . Right =<< case payload of
                            Just x  -> customPayloadMethodWith method authHeader (stdUrl ++ path) x
                            Nothing -> customMethodWith method authHeader (stdUrl ++ path)

                        handler :: HttpException -> IO (Either (Response ()) (Response ByteString))
                        handler (HttpExceptionRequest _ (StatusCodeException resp _)) = return (Left resp)  --Converts the internal exception data type of wreq to something we can more easily handle.



                continue' :: (SupportsNetworkError e, FromJSON v) => String -> HashMap String (Bool, Int) -> String -> ActionMonad e s v
                continue' s mp parampath = do
                    case HM.lookup s mp of      -- Check HashMap to see if we're rate limited for this parameter.
                        Just (True, reset) ->
                                let diff = (reset - time)
                                in
                                    if diff >= 0 then do
                                        liftIO (putStrLn $ "Locally rate limited. (According to state). Have to wait for " ++ show diff ++ " seconds.")
                                        throwError . fromBR $ RateLimited False diff
                                    else
                                        continue''
                        _                   -> continue''

                    where
                        continue'' :: (SupportsNetworkError e, FromJSON v) => ActionMonad e s v
                        continue'' = do
                            result <- liftIO $ catch performRequest handler

                            case result of
                                (Right resp) -> do
                                    st <- get
                                    case param of -- The request went through. Study the response to find out about rate limits, and update state accordingly.
                                        (Guild _)   -> put st{rateLimitGuilds=(storeRateLimits resp)}
                                        (Channel _) -> put st{rateLimitChannels=(storeRateLimits resp)}
                                    return $ convertResponse resp       -- Convert the body of response to a FromJSON value.
                                (Left resp) -> liftIO epoch >>= \time -> --update time. TODO: Rather than recheck time, use timestamp header
                                    case resp ^. responseStatus ^. statusCode of
                                            400 ->
                                                throwError . fromBR $ BR400
                                            429 | (resp ^? responseHeader "X-RateLimit-Global") == Just "true" -> do
                                                liftIO $ putStrLn "Globally rate limited. (429 response)"
                                                let retryafter = (read . BC8.unpack $ resp ^. responseHeader "Retry-After") :: Int
                                                get >>= \st -> put st{rateLimitGlobal=(Just(time + retryafter))}
                                                throwError . fromBR $ RateLimited True retryafter
                                            429 -> do   -- We ran into a 429 response about the local rate limit! This should NEVER HAPPEN! customRequest uses state to keep local rate limits in check.
                                                liftIO $ putStrLn "Locally rate limited. (429 response) ***** THIS SHOULD NEVER HAPPEN! REPORT THIS! *****"
                                                let reset = read . BC8.unpack $ resp ^. responseHeader "X-RateLimit-Reset" :: Int
                                                let newrate = HM.insert s (True, reset) mp
                                                st <- get
                                                case param of
                                                    (Guild _) -> put st{rateLimitGuilds=newrate}
                                                    (Channel _) -> put st{rateLimitChannels=newrate}
                                                throwError . fromBR $ RateLimited False (reset - time)
                                            x -> do
                                                throwError . fromBR $ OtherResponse x resp

                        performRequest :: IO (Either (Response ()) (Response ByteString))
                        performRequest = fmap Right $ case payload of
                            Just x  -> customPayloadMethodWith method authHeader thepath x
                            _       -> customMethodWith method authHeader thepath
                            where
                                thepath = (stdUrl ++ parampath ++ s ++ path)

                        handler :: HttpException -> IO (Either (Response ()) (Response ByteString))
                        handler (HttpExceptionRequest _ (StatusCodeException resp _)) = return (Left resp)

                        -- Reads local rate limits from a response, and updates the rate limit HashMap in scope.
                        storeRateLimits :: Response ByteString -> HashMap String (Bool, Int)
                        storeRateLimits resp =
                            case resp ^? responseHeader "X-RateLimit-Remaining" of
                                Just x  ->
                                    let reset = read . BC8.unpack $ resp ^. responseHeader "X-RateLimit-Reset" :: Int
                                    in HM.insert s (x == "0", reset) mp
                                _       -> mp



----------------------------------------------------------------------------------------------------------------------------------------------------------
----- This is a big slew of predefined HTTP requests for the Discord API. It's not quite exhaustive, because I'm lazy, but it's very close to it. --------
----------------------------------------------------------------------------------------------------------------------------------------------------------


createMessage :: (SupportsNetworkError e, FromJSON v) => String -> String -> Bool -> ActionMonad e s v
createMessage channel content withTTS = customRequest (Channel channel) "/messages" "POST" $ Just (object ["content" .= content, "tts" .= withTTS])

getChannel :: (SupportsNetworkError e, FromJSON v) => String -> ActionMonad e s v
getChannel channel = customRequest (Channel channel) "" "GET" Nothing

modifyChannel :: (SupportsNetworkError e, FromJSON v) => String -> Value -> ActionMonad e s v
modifyChannel channel obj = customRequest (Channel channel) "" "PATCH" $ Just obj

deleteChannel :: (SupportsNetworkError e, FromJSON v) => String -> ActionMonad e s v
deleteChannel channel = customRequest (Channel channel) "" "DELETE" Nothing

getChannelMessages :: (SupportsNetworkError e, FromJSON v) => String -> Int -> ActionMonad e s v
getChannelMessages channel limit = customRequest (Channel channel) ("/messages?limit=" ++ (show limit)) "GET" Nothing

getChannelMessagesAround :: (SupportsNetworkError e, FromJSON v) => String -> Int -> String -> ActionMonad e s v
getChannelMessagesAround channel limit message = customRequest (Channel channel) ("/messages?limit=" ++ (show limit) ++ "&around=" ++ message) "GET" Nothing

getChannelMessagesBefore :: (SupportsNetworkError e, FromJSON v) => String -> Int -> String -> ActionMonad e s v
getChannelMessagesBefore channel limit message = customRequest (Channel channel) ("/messages?limit=" ++ (show limit) ++ "&before=" ++ message) "GET" Nothing

getChannelMessagesAfter :: (SupportsNetworkError e, FromJSON v) => String -> Int -> String -> ActionMonad e s v
getChannelMessagesAfter channel limit message = customRequest (Channel channel) ("/messages?limit=" ++ (show limit) ++ "&after=" ++ message) "GET" Nothing

getChannelMessage :: (SupportsNetworkError e, FromJSON v) => String -> String -> ActionMonad e s v
getChannelMessage channel message = customRequest (Channel channel) ("/messages/" ++ message) "GET" Nothing

editMessage :: (SupportsNetworkError e, FromJSON v) => String -> String -> Value -> ActionMonad e s v
editMessage channel message content = customRequest (Channel channel) ("/messages/" ++ message) "PATCH" $ Just content

deleteMessage :: (SupportsNetworkError e, FromJSON v) => String -> String -> ActionMonad e s v
deleteMessage channel message = customRequest (Channel channel) ("/messages/" ++ message) "DELETE" Nothing

bulkDeleteMessages :: (SupportsNetworkError e, FromJSON v) => String -> [String] -> ActionMonad e s v
bulkDeleteMessages channel messages = customRequest (Channel channel) ("/messages/bulk-delete") "POST" $ Just (object ["messages" .= messages])

editChannelPermissions :: (SupportsNetworkError e, FromJSON v) => String -> String -> Value -> ActionMonad e s v
editChannelPermissions channel overwrite params = customRequest (Channel channel) ("/permissions/" ++ overwrite) "PUT" $ Just params

createChannelInvite :: (SupportsNetworkError e, FromJSON v) => String -> Value -> ActionMonad e s v
createChannelInvite channel params = customRequest (Channel channel) ("/invites") "POST" $ Just params

deleteChannelPermissions :: SupportsNetworkError e => String -> String -> ActionMonad e a ()
deleteChannelPermissions channel overwrite = void . asValue $ customRequest (Channel channel) ("/permissions/" ++ overwrite) "DELETE" Nothing

createGuild :: (SupportsNetworkError e, FromJSON v) => Value -> ActionMonad e s v
createGuild guild = customRequest None ("/guilds") "POST" $ Just guild

getGuild :: (SupportsNetworkError e, FromJSON v) => String -> ActionMonad e s v
getGuild guild = customRequest (Guild guild) ("") "GET" Nothing

modifyGuild :: (SupportsNetworkError e, FromJSON v) => String -> Value -> ActionMonad e s v
modifyGuild guild param = customRequest (Guild guild) ("") "PATCH" $ Just param

deleteGuild :: (SupportsNetworkError e, FromJSON v) => String -> ActionMonad e s v
deleteGuild guild = customRequest (Guild guild) ("") "DELETE" Nothing

getGuildChannels :: (SupportsNetworkError e, FromJSON v) => String -> ActionMonad e s v
getGuildChannels guild = customRequest (Guild guild) ("/channels") "GET" Nothing

createGuildChannel :: (SupportsNetworkError e, FromJSON v) => String -> Value -> ActionMonad e s v
createGuildChannel guild channel = customRequest (Guild guild) ("/channels") "POST" $ Just channel

getGuildMember :: (SupportsNetworkError e, FromJSON v) => String -> String -> ActionMonad e s v
getGuildMember guild user = customRequest (Guild guild) ("/members/" ++ user) "GET" Nothing

listGuildMembers :: (SupportsNetworkError e, FromJSON v) => String -> ActionMonad e s v
listGuildMembers guild = customRequest (Guild guild) ("/members") "GET" Nothing

addGuildMember :: (SupportsNetworkError e, FromJSON v) => String -> String -> Value -> ActionMonad e s v
addGuildMember guild user param = customRequest (Guild guild) ("/members/" ++ user) "PUT" $ Just param

modifyGuildMember :: (SupportsNetworkError e, FromJSON v) => String -> String -> Value -> ActionMonad e s v
modifyGuildMember guild user param = customRequest (Guild guild) ("/members/" ++ user) "PUT" $ Just param

modifyCurrentUsersNick :: (SupportsNetworkError e, FromJSON v) => String -> String -> ActionMonad e s v
modifyCurrentUsersNick guild nick = customRequest (Guild guild) ("/members/@me/nick") "PATCH" $ Just (object ["nick" .= nick])

addGuildMemberRole :: (SupportsNetworkError e, FromJSON v) => String -> String -> String -> ActionMonad e s v
addGuildMemberRole guild user role = customRequest (Guild guild) ("/members/" ++ user ++ "/roles/" ++ role) "PUT" Nothing

removeGuildMemberRole :: (SupportsNetworkError e, FromJSON v) => String -> String -> String -> ActionMonad e s v
removeGuildMemberRole guild user role = customRequest (Guild guild) ("/members/" ++ user ++ "/roles/" ++ role) "DELETE" Nothing

removeGuildMember :: (SupportsNetworkError e, FromJSON v) => String -> String -> ActionMonad e s v
removeGuildMember guild user = customRequest (Guild guild) ("/members/" ++ user) "DELETE" Nothing

getGuildBans :: (SupportsNetworkError e, FromJSON v) => String -> ActionMonad e s v
getGuildBans guild = customRequest (Guild guild) ("/bans") "GET" Nothing

createGuildBan :: (SupportsNetworkError e, FromJSON v) => String -> String -> Int -> ActionMonad e s v
createGuildBan guild user days = customRequest (Guild guild) ("/bans/" ++ user) "PUT" $ Just (object ["delete-message-days" .= days])

removeGuildBan :: (SupportsNetworkError e, FromJSON v) => String -> String -> ActionMonad e s v
removeGuildBan guild user = customRequest (Guild guild) ("/bans/" ++ user) "DELETE" Nothing

getGuildRoles :: (SupportsNetworkError e, FromJSON v) => String -> ActionMonad e s v
getGuildRoles guild = customRequest (Guild guild) ("/roles") "GET" Nothing

createGuildRole :: (SupportsNetworkError e, FromJSON v) => String -> Value -> ActionMonad e s v
createGuildRole guild param = customRequest (Guild guild) ("/roles") "POST" $ Just param

modifyGuildRole :: (SupportsNetworkError e, FromJSON v) => String -> String -> Value -> ActionMonad e s v
modifyGuildRole guild role param = customRequest (Guild guild) ("/roles/" ++ role) "PATCH" $ Just param


getGuildPruneCount :: (SupportsNetworkError e, FromJSON v) => String -> Value -> ActionMonad e s v
getGuildPruneCount guild days = customRequest (Guild guild) ("/prune") "GET" $ Just (object ["days" .= days])

beginGuildPrune :: (SupportsNetworkError e, FromJSON v) => String -> Value -> ActionMonad e s v
beginGuildPrune guild days = customRequest (Guild guild) ("/prune") "POST" $ Just (object ["days" .= days])



---------------------------------------
----- PERMISSION RELATED STUFF --------
---------------------------------------

-- The Permissions data type. Holds bool for every permission. Alternatively, Admin indicates all permissions.
data Permissions =
        Permissions
            {
                createInstantInvite :: Bool
            ,   kickMembers         :: Bool
            ,   banMembers          :: Bool
            ,   manageChannels      :: Bool
            ,   manageGuilds        :: Bool
            ,   addReactions        :: Bool
            ,   readMessages        :: Bool
            ,   sendMessages        :: Bool
            ,   sendTTSMessages     :: Bool
            ,   manageMessages      :: Bool
            ,   embedLinks          :: Bool
            ,   attachFiles         :: Bool
            ,   readMessageHistory  :: Bool
            ,   mentionEveryone     :: Bool
            ,   useExternalEmojis   :: Bool
            ,   connect             :: Bool
            ,   speak               :: Bool
            ,   muteMembers         :: Bool
            ,   deafenMembers       :: Bool
            ,   moveMembers         :: Bool
            ,   useVAD              :: Bool
            ,   changeNickname      :: Bool
            ,   manageNicknames     :: Bool
            ,   manageRoles         :: Bool
            ,   manageWebhooks      :: Bool
            ,   manageEmojis        :: Bool
            }
    |   Admin

defaultPermissions :: Permissions
defaultPermissions =
    Permissions
        {
            createInstantInvite = False
        ,   kickMembers         = False
        ,   banMembers          = False
        ,   manageChannels      = False
        ,   manageGuilds        = False
        ,   addReactions        = False
        ,   readMessages        = False
        ,   sendMessages        = False
        ,   sendTTSMessages     = False
        ,   manageMessages      = False
        ,   embedLinks          = False
        ,   attachFiles         = False
        ,   readMessageHistory  = False
        ,   mentionEveryone     = False
        ,   useExternalEmojis   = False
        ,   connect             = False
        ,   speak               = False
        ,   muteMembers         = False
        ,   deafenMembers       = False
        ,   moveMembers         = False
        ,   useVAD              = False
        ,   changeNickname      = False
        ,   manageNicknames     = False
        ,   manageRoles         = False
        ,   manageWebhooks      = False
        ,   manageEmojis        = False
        }

-- This allows Value-as-numbers to be converted straight to and from Permissions, however, this isn't really used in practice.
-- This is really only there so you may make a StateValue instance of Permissions, and then memoize them.
instance FromJSON Permissions where
    parseJSON = withScientific "Permissions" $ \numb ->
        pure $ updatePermissions True (floor numb) defaultPermissions

instance ToJSON Permissions where
    toJSON Admin = toJSON (0x8 :: Int)
    toJSON Permissions{..} = toJSON $
        sum $ concat
                [
                    zipWith (\p b -> fromEnum b `shiftL` p) [0..6]
                        [
                            createInstantInvite
                        ,   kickMembers
                        ,   banMembers
                        ,   manageChannels
                        ,   manageGuilds
                        ,   addReactions
                        ]
                ,   zipWith (\p b -> fromEnum b `shiftL` p) [10..18]
                        [
                            readMessages
                        ,   sendMessages
                        ,   sendTTSMessages
                        ,   manageMessages
                        ,   embedLinks
                        ,   attachFiles
                        ,   readMessageHistory
                        ,   mentionEveryone
                        ,   useExternalEmojis
                        ]
                ,   zipWith (\p b -> fromEnum b `shiftL` p) [20..30]
                        [
                            connect
                        ,   speak
                        ,   muteMembers
                        ,   deafenMembers
                        ,   moveMembers
                        ,   useVAD
                        ,   changeNickname
                        ,   manageNicknames
                        ,   manageRoles
                        ,   manageWebhooks
                        ,   manageEmojis
                        ]
                ]

-- This function shouldn't actually get exported. Given an permission set number, and a Permissions, updates the Permissions according to the bool
-- if the bool is True, is signals that the permission set number is to add permissions. If the bool is false, it signals that the permission set number is to remove permissions.
updatePermissions :: Bool -> Int -> Permissions -> Permissions
updatePermissions allow permission oldp =
    let
        (|?) :: Bool -> Int -> Bool
        old |? int
            | allow     = old || (testBit permission int)
            | otherwise = old && not (testBit permission int)
    in  
        if testBit permission 3 || isAdmin oldp
            then Admin
            else 
                Permissions
                        {
                            createInstantInvite = createInstantInvite   oldp |? 0
                        ,   kickMembers         = kickMembers           oldp |? 1
                        ,   banMembers          = banMembers            oldp |? 2
                        ,   manageChannels      = manageChannels        oldp |? 4
                        ,   manageGuilds        = manageGuilds          oldp |? 5
                        ,   addReactions        = addReactions          oldp |? 6
                        ,   readMessages        = readMessages          oldp |? 10
                        ,   sendMessages        = sendMessages          oldp |? 11
                        ,   sendTTSMessages     = sendTTSMessages       oldp |? 12
                        ,   manageMessages      = manageMessages        oldp |? 13
                        ,   embedLinks          = embedLinks            oldp |? 14
                        ,   attachFiles         = attachFiles           oldp |? 15
                        ,   readMessageHistory  = readMessageHistory    oldp |? 16
                        ,   mentionEveryone     = mentionEveryone       oldp |? 17
                        ,   useExternalEmojis   = useExternalEmojis     oldp |? 18
                        ,   connect             = Bot.connect           oldp |? 20
                        ,   speak               = speak                 oldp |? 21
                        ,   muteMembers         = muteMembers           oldp |? 22
                        ,   deafenMembers       = deafenMembers         oldp |? 23
                        ,   moveMembers         = moveMembers           oldp |? 24
                        ,   useVAD              = useVAD                oldp |? 25
                        ,   changeNickname      = changeNickname        oldp |? 26
                        ,   manageNicknames     = manageNicknames       oldp |? 27
                        ,   manageRoles         = manageRoles           oldp |? 28
                        ,   manageWebhooks      = manageWebhooks        oldp |? 29
                        ,   manageEmojis        = manageEmojis          oldp |? 30
                        }

isAdmin :: Permissions -> Bool
isAdmin Admin   = True
isAdmin _       = False

-- This should be used rather than the actual record field functions, as this covers the cases when the Permissions is Admin.
hasPermission :: Permissions -> (Permissions -> Bool) -> Bool
hasPermission p func    = isAdmin p || func p

-- A pure function to determine a member's permissions given an array of the guild's roles, and an array of the ID's of the roles the member possesses.
determineGuildMemberPermissions :: Vector Value -> Vector Text -> Permissions
determineGuildMemberPermissions guildroles memberroles =
    let
        findAndUpdate :: Text -> Permissions -> Permissions
        findAndUpdate rid next =
            case find ((==) rid . flip filterJSON "id") guildroles of
                Just a  -> updatePermissions True (filterJSON a "permissions") next
                Nothing -> next
    in
        Prelude.foldr findAndUpdate defaultPermissions memberroles

-- A pure function to determine a member's permissions within a channel given  an array of the guild's roles,
-- an array of permission overwrite objects for the channel, and a guild member object.
determineChannelMemberPermissions :: Vector Value -> Vector Value -> Value -> Permissions
determineChannelMemberPermissions guildroles overwrites member
    | isAdmin guildpermissions  = Admin
    | otherwise                 = Prelude.foldr findAndUpdate guildpermissions overwrites
    where
        memberid            :: Text
        memberid            = filterJSON member "id"

        memberroles         :: Vector Text
        memberroles         = filterJSON member "roles"

        guildpermissions    :: Permissions
        guildpermissions    = determineGuildMemberPermissions guildroles memberroles


        findAndUpdate                   :: (Value -> Permissions -> Permissions)
        findAndUpdate overwrite next    =
            let
                anid :: Text
                anid =  filterJSON overwrite "id"

                atype :: Text
                atype =  filterJSON overwrite "type"

                allow :: Int
                allow =  filterJSON overwrite "allow"

                deny :: Int
                deny =  filterJSON overwrite "deny"

            in
                if
                    case atype of
                        "role"  -> Data.Vector.elem anid memberroles
                        _       -> memberid == anid

                    then updatePermissions False deny $ updatePermissions True allow next
                    else next

-- Uses IO to get the permissions for a guild member. Has no sort of memoize built in.
getGuildMemberPermissions :: (SupportsNetworkError e) => String -> String -> ActionMonad e s Permissions
getGuildMemberPermissions gid uid = do
    guild   <- getGuild gid
    member  <- getGuildMember gid uid
    return $
        let
            guildroles      :: Vector Value
            guildroles      = filterJSON guild "roles"
            memberroles     :: Vector Text
            memberroles     = filterJSON member "roles"
        in
            determineGuildMemberPermissions guildroles memberroles

-- A variant that allows the user to provide functions that modify the getGuild and getRequest functions, respectively.
-- This allows for memoization, or wait for rate limits, etc.
getGuildMemberPermissions' ::
        (SupportsNetworkError e) =>
        (ActionMonad e s Value -> ActionMonad e s Value)
    ->  (String -> ActionMonad e s Value -> ActionMonad e s Value)
    ->  String -> String -> ActionMonad e s Permissions
getGuildMemberPermissions' gf mf gid uid = do 
    guild   <- gf (getGuild gid)
    member  <- mf gid (getGuildMember gid uid)
    return $
        let
            guildroles      :: Vector Value
            guildroles      = filterJSON guild "roles"
            memberroles     :: Vector Text
            memberroles     = filterJSON member "roles"
        in
            determineGuildMemberPermissions guildroles memberroles
        
getChannelMemberPermissions :: (SupportsNetworkError e) => String -> String -> ActionMonad e s Permissions
getChannelMemberPermissions cid uid = do
    channel <- getChannel cid
    let 
        gid :: String
        gid = filterJSON channel "guild_id"
    guild   <- getGuild gid
    member  <- getGuildMember gid uid
    return $
        let
            guildroles      :: Vector Value
            guildroles      = filterJSON guild "roles"

            overwrites      :: Vector Value
            overwrites      = filterJSON channel "permission_overwrites"

        in
            determineChannelMemberPermissions guildroles overwrites member

getChannelMemberPermissions' ::
        (SupportsNetworkError e) =>
        (ActionMonad e s Value -> ActionMonad e s Value)
    ->  (String -> ActionMonad e s Value -> ActionMonad e s Value)
    ->  (String -> ActionMonad e s Value -> ActionMonad e s Value)
    ->  String -> String -> ActionMonad e s Permissions
getChannelMemberPermissions' cf gf mf cid uid = do
    channel <- cf (getChannel cid)
    let 
        gid :: String
        gid = filterJSON channel "guild_id"
    guild   <- gf gid (getGuild gid)
    member  <- mf gid (getGuildMember gid uid)
    return $
        let
            guildroles      :: Vector Value
            guildroles      = filterJSON guild "roles"

            overwrites      :: Vector Value
            overwrites      = filterJSON channel "permission_overwrites"

        in
            determineChannelMemberPermissions guildroles overwrites member



-- RunResult is the return type of catchInitial; indicating how the bot was shutdown, and whether it should reconnect.
-- Retry indicates it should attempt to reconnect to a session, Abandon means it should reconnect with a completely fresh session,
-- and Done means that the bot has shutdown naturally, and the program should terminate.
data RunResult a = Retry (Session a) | Abandon | Done


data Session a = Continue String String Int [(Bool, Action a)] | Fresh


launchBot :: BotStateInstance a => String -> Behaviour a -> IO ()
launchBot = bot Fresh

-- the main function everything boils down to, however, launchBot is still what should be used to launch the bot.
bot :: BotStateInstance a => Session a -> String -> Behaviour a -> IO ()
bot session token behaviour =
    catchInitial 1 >>= \result ->
        case result of
            Retry session -> bot session token behaviour
            Abandon     -> bot Fresh token behaviour
            _           -> return ()
    where
        catchInitial tries = catchIOError
            (putStrLn ((if notFresh then "Rec" else "C") ++ "onnecting to gateway...") >> 
                runSecureClient "gateway.discord.gg" 443 "/?v=5&encoding=json" initialize) -- As runSecureClient throws IO exceptions when it runs into stuff, we need to catch it.
            (\error@(IOError {ioe_type}) ->
                case ioe_type of
                    NoSuchThing -> do -- This error type indicate that the URL wasn't recognized - i.e., the bot's net is down.
                        putStrLn ("Connection failed. Waiting for " ++ show (cap ^ 2) ++ " seconds before retry...") -- Tries to reconnect indefinitely, however, the time it waits between each event
                        continue <- newEmptyMVar                                                                     -- Increases squarically. Caps out at 625 seconds ≈ 10.4 minutes.
                        waitthreads <- newEmptyMVar                                                                  -- The user may terminate this process using "quit"
                        createThread waitthreads $ waitForTimer continue
                        createThread waitthreads $ waitForUser continue
                        cont <- takeMVar continue
                        murder waitthreads
                        if cont
                            then
                                performGC >>
                                (catchInitial (cap + 1))
                            else
                                return Done
                    _           -> ioError error
            )

            where
                notFresh :: Bool
                notFresh = case session of
                    Fresh   -> False
                    _       -> True

                cap | tries > 25   = 25
                    | otherwise  = tries

                waitForTimer :: MVar Bool -> IO ()
                waitForTimer continue = do
                    threadDelay (1000000 * cap ^ 2)
                    void $ tryPutMVar continue True

                waitForUser :: MVar Bool -> IO ()
                waitForUser continue = do
                    input <- getLine
                    if "quit" `List.isInfixOf` input
                        then
                            putStrLn "Shutting down..." >> void (tryPutMVar continue False)
                        else
                            waitForUser continue

        murder :: MVar [ThreadId] -> IO ()
        murder threads = do
            tokill <- takeMVar threads
            void $ mapM (\t -> killThread t) tokill

        emergencyshutdown :: Show e => MVar Bool -> String -> e -> IO ()
        emergencyshutdown toquit site = \e -> do
            putStrLn $ "*** Unhandled IO Exception in " ++ site ++ "Given error is: " ++ show e ++ " ***"
            void $ tryPutMVar toquit False

        createThread :: MVar [ThreadId] -> IO () -> IO ()
        createThread threads function = do
            e <- isEmptyMVar threads
            if e  -- in first thread, use putMVar rather than modifyMVar_
                then (\thread -> putMVar     threads [thread]                   ) =<< (forkIO $ function) 
                else (\thread -> modifyMVar_ threads (\q -> return $ thread : q)) =<< (forkIO $ function)

        --initialize :: Connection -> IO (RunResult a)
        initialize connection = do
            putStrLn "Waiting for Hello payload..."
            Hello{interval} <- getJSON connection
            -- Authenticate, and potentially resume.
            putStrLn "Handshaking..."
            connect <- initConn
            case connect of
                Nothing -> 
                    putStrLn "Invalid session. Creating a new one..." >>
                    sendClose connection ("" :: ByteString) >>
                    return Abandon
                Just (session_id, bot_id, hbseq, actionQueue) -> do
                    putStrLn "Handshake complete."
                    state <- initState bot_id -- The state of the bot
                    putStrLn "Connected."
                    --sendPayload connection $ Status "ENDLESS TRASH"
        
                    hback           <- newMVar ()           -- Indicates whether last heartbeat has been acknowledged
                    refer           <- newEmptyMVar         -- Indicates if payloads should be referred to actions.
                    vwaiting        <- newEmptyMVar         -- Indicates whether voice is waiting for the gateway to respond with voice related wvents.
                    vresult         <- newEmptyMVar         -- The result of the voice related events.
                    vtransmitting   <- newEmptyMVar         -- Indicates whether the bot is currently transmitting voice.
                    quit            <- newEmptyMVar         -- Indicates if the program should quit. True means it will try to reconnect. False will make it simply quit.
                    threads         <- newEmptyMVar         -- Threads to kill when the program terminates.
                    
                    -- Create the various threads; start listening and reacting, using a consumer-producer model
                    -- TODO: IMPLEMENT READERT FOR GODS SAKE THESE ARGUMENTS ARE RIDICULOUS
                    createThread threads $ sendHeartbeats hbseq hback quit connection interval
                    createThread threads $ produceForever vwaiting vresult refer actionQueue hbseq hback quit connection
                    createThread threads $ consumeForever vwaiting vresult vtransmitting bot_id threads refer actionQueue hbseq hback quit connection state
                    createThread threads $ userInput quit state
                    when (timer behaviour > 0) $ createThread threads $ runTimer actionQueue -- a thread for timer will only be created when timer > 0.
        
                    waitforQuit quit threads actionQueue hbseq state session_id bot_id
            

            where
                -- Creates initial state. Will base internal state on .txt file with the same name as the behaviour. If the file is nonexistant or faulty, will default to defaulState as defined by behaviour
                initState bot_id =
                    catchIOError
                        (do
                            handle <- openFile (name behaviour ++ ".txt") ReadMode
                            internalstate <- hGetContents handle
                            case readMaybe internalstate of
                                Just x  -> hClose handle >> putStrLn "Read from existing state." >> newMVar (initstate x authHeader bot_id)
                                Nothing -> hClose handle >> ioError (userError "File corrupt")
                        )
                        (\_ -> do
                            putStrLn "State file doesn't exist or is corrupt. Creating new..."
                            writeFile (name behaviour ++ ".txt") (show (defaultState behaviour))
                            newMVar (initstate (defaultState behaviour) authHeader bot_id)
                        )
                    where
                        authHeader = (defaults & header "Authorization" .~ [BC8.pack ("Bot " ++ token)] & header "User-Agent" .~ ["DiscordBot (https://github.com/matte216/TrashBot, alpha)"])

                -- Fetches initial actionQueue, heartbeat sequence, and session id. The process depends on the Session.
                initConn =
                    case session of
                        Continue session_id bot_id sq aQ -> retry session_id bot_id sq aQ
                        _ -> fresh
                    where
                        retry session_id bot_id sq aQ = do
                            putStrLn "Replaying events..."
                            sendJSON connection $ Resume token session_id sq
                            getUntilResumed session_id bot_id aQ

                        getUntilResumed session_id bot_id aQ = do
                            payload <- getJSON connection
                            case payload of
                                Dispatch{eventName, eventData, sq} -> 
                                    if eventName == "RESUMED"
                                        then do
                                            actionQueue <-
                                                if null aQ
                                                    then
                                                        newEmptyMVar
                                                    else
                                                        newMVar $ aQ ++ [(True, resumed (reactions behaviour) eventData)]
                                            hbseq <- newMVar sq
                                            putStrLn "Finished replaying events."
                                            return $ Just (session_id, bot_id, hbseq, actionQueue)
                                        else
                                            getUntilResumed session_id bot_id $ aQ ++ [(True, react behaviour eventName eventData)]
                                InvalidSession -> return Nothing
                                _ -> error "THIS SHOULD NOT HAPPEN. See getUntilResumed"

                        --fresh :: IO (Maybe (String, MVar Int, MVar [(Bool, Action a)]))
                        fresh = do
                            sendJSON connection Identify{eventData = object [
                                      "token" .= token
                                    , "properties" .= object []
                                    , "compress" .= False
                                    , "large_threshold" .= (50 :: Int)
                                    , "shard" .= ((0, 1) :: (Int, Int))
                                    ]
                                }
                            Dispatch{sq, eventData} <- getJSON connection
                            let session_id = filterJSON eventData "session_id" :: String
                            let bot_id = filterJSON (filterJSON eventData "user") "id" :: String
                            actionQueue <- newMVar [(False, ready (reactions behaviour) eventData)]
                            hbseq <- newMVar sq
                            return $ Just (session_id, bot_id, hbseq, actionQueue)

                -- Now waits for quit MVar, and quits depending on the bool. If True, it will try to resume connection. If False, it will end the program.
                waitforQuit toquit threads actionQueue hbseq state session_id bot_id = do
                    quit <- takeMVar toquit
                    if quit
                        then do
                            murder threads
                            storestate
                            sq <- readMVar hbseq
                            aQ <- tryTakeMVar actionQueue
                            sendCloseCode connection 1001 ("" :: ByteString)
                            return $ Retry (
                                case aQ of
                                    Just ls -> Continue session_id bot_id sq [(True, action) | (_, action) <- ls]
                                    _       -> Continue session_id bot_id sq []
                                )
                        else do
                            putStrLn "Shutting down..."
                            sendClose connection ("" :: ByteString)
                            murder threads
                            storestate
                            return Done
                    where
                        storestate :: IO ()
                        storestate = do
                            BotState{internal} <- takeMVar state
                            writeFile ((name behaviour) ++ ".txt") (show internal)

                -- Reads user input.
                userInput quit state = do
                    input <- getLine
                    case () of
                        _ | "quit" `List.isInfixOf` input   -> void $ tryPutMVar quit False
                          | "clear" `List.isInfixOf`input   -> modifyMVar_ state (\st -> return st{internal = (defaultState behaviour)}) >> userInput quit state
                          | otherwise               -> userInput quit state

                {- enqueueInMVar mvar item
                    PRE: True
                    POST: None
                    SIDE-EFFECTS:
                        Queues an item in the mvar.
                -}
                enqueueInMVar :: MVar [a] -> a -> IO ()
                enqueueInMVar mvar item = do
                    empty <- isEmptyMVar mvar
                    if empty
                        then putMVar mvar [item]
                        else modifyMVar_ mvar (\q -> return $ q ++ [item]) -- The action is appended to satisfy FIFO.

                {- runTimer actionQueue
                    PURPOSE: adds onTimer action provided by behaviour to actionQueue at regular intervals.
                    PRE: timer, as defined by the behaviour, is greater than 0.
                    POST: True 
                    SIDE-EFFECTS:
                        Constantly queues the onTimer action provided by behaviour at intervals of i seconds, where i is timer.
                -}
                --runTimer :: MVar [(Bool, Action a)] -> IO ()
                runTimer actionQ = forever $ do
                    enqueueInMVar actionQ (False, onTimer behaviour)
                    threadDelay $ (timer behaviour) * 1000000
                    

                {- sendHeartbeats heartbeatSequence connection interval
                   PURPOSE: Send heartbeats at regular intervals to keep the connection alive
                   PRE: Connection is an open, authenticated SSL connection to gateway.discord.gg
                   POST: True
                   SIDE-EFFECTS: Regularly send heartbeat payloads to connection
                -}
                sendHeartbeats :: MVar Int -> MVar () -> MVar Bool -> Connection -> Int -> IO ()
                sendHeartbeats hbseq hback quit connection interval = flip catchIOError (emergencyshutdown quit "sendHeartbeats") $ forever $ do
                    hb <- readMVar hbseq
                    sendJSON connection Heartbeat{sq=hb}
                    void $ tryTakeMVar hback
                    waitForAck 10
                    performGC
                    threadDelay (interval * 1000)
                    where
                        -- Waits for acknowledgement for 5 seconds.
                        waitForAck :: Int -> IO ()
                        waitForAck 0 = putStrLn "Connection lost. Trying to resume..." >> void (tryPutMVar quit True)
                        waitForAck i = isEmptyMVar hback >>= (\waiting -> 
                            when waiting (do
                                threadDelay 500000
                                waitForAck (i - 1)
                                )
                            )


                {- produceForever actionQueue heartbeatSequence connection
                   PURPOSE: Listen coninuously for events, adding reactions to eventQueue when they happen.
                   PRE: Connection is an open, authenticated SSL connection to gateway.discord.gg
                   POST: True
                   SIDE-EFFECTS:
                        Receives data from connection continuously.
                        Whenever a Dispatch payload is received:
                            the heartbeat sequence is updated,
                            a reaction to the event that the payload represents is added to the actionQueue.
                -}
                {-
                --produceForever :: MVar [(Bool, Action a)] -> MVar Int -> MVar () -> MVar Bool -> Connection -> IO ()
                produceForever refer actionQ hbseq hback quit connection = forever $ do
                    pl <- getPayload connection
                    action <- tryReadMVar refer
                    case action of
                        Just (x:_)  -> dequeueMVar refer >> enqueueInMVar actionQ (False, x pl)
                        _           -> handlePayload actionQ hbseq hback quit connection pl
                -}

                produceForever vwaiting vresult refer actionQ hbseq hback quit connection = flip catchIOError (emergencyshutdown quit "produceForever") $ forever $ do
                    pl <- getJSON connection
                    action <- tryReadMVar refer
                    voice <- tryReadMVar vwaiting
                    case voice of                               -- See if voiceTransmit excepts some voice-related payloads.
                        Nothing     -> continue action pl       -- We don't, so continue.
                        Just (a, b) ->                          -- We do!
                            case pl of
                                Dispatch{eventName, eventData, sq}  -> 
                                    let
                                        new :: Maybe (Maybe String, Maybe (String, String, String))
                                        new = case eventName of                             
                                            "VOICE_STATE_UPDATE"    -> Just (lookupJSON eventData "session_id", b)
                                            "VOICE_SERVER_UPDATE"   -> Just (a, Just (filterJSON eventData "token", filterJSON eventData "guild_id", filterJSON eventData "endpoint"))
                                            _                       -> Nothing
                                    in
                                        case new of
                                            Just (Just s, Just (t,u,v)) -> swapMVar hbseq sq >> void (takeMVar vwaiting) >> putMVar vresult (s, t, u, v)    -- If we've received both payloads, put the result for transmitVoice to use, and empty the vwaiting MVar.
                                            Just x                      -> swapMVar hbseq sq >> void (swapMVar vwaiting x)                                  -- If we're still waiting for one payload, update the vwaiting MVar with the info we've just received.
                                            _                           -> continue action pl                                                               -- It was some other event. Pass it on.
                                _                                   -> continue action pl   --Not a Dispatch payload. Pass it on.
                    where
                        continue action pl = case action of
                                Just (x:_)  -> dequeueMVar refer >> enqueueInMVar actionQ (False, x pl)
                                _           -> handlePayload actionQ hbseq hback quit connection pl        


                handlePayload actionQ hbseq hback quit connection pl = case pl of
                    Dispatch{eventName, eventData, sq} -> do
                        let action = react behaviour eventName eventData
                        swapMVar hbseq sq
                        enqueueInMVar actionQ (False, action)
                    Heartbeat{} -> readMVar hbseq >>= void . (\hb -> sendJSON connection Heartbeat{sq=hb})
                    Reconnect -> putStrLn "Requested to reconnect." >> void (tryPutMVar quit True)
                    HeartbeatAck -> do
                        void $ tryPutMVar hback ()
                    _ -> return ()

                {- consumeForever actionQueue state
                   PURPOSE: Continuously dequeues actions, updating state throughout.
                   PRE: True
                   POST: True
                   SIDE-EFFECTS:
                        Whenever the actionQueue is non-Empty, for each action in the queue:
                            Dequeue that action and perform it, updating state to the one that the action returns.
                -}

                dequeueMVar :: MVar [a] -> IO ()
                dequeueMVar q = takeMVar q >>= dequeue' q
                    where
                        dequeue' :: MVar [a] -> [a] -> IO ()
                        dequeue' q [_] = return ()
                        dequeue' q (_:rest) = putMVar q rest

                consumeForever vwaiting vresult vtransmitting bot_id threads refer actionQ hbseq hback quit connection state = flip catchIOError (emergencyshutdown quit "consumeForever") $ do
                    ((retro, action):_) <- readMVar actionQ -- Waits until the action queue is nonempty, and reads action to perform. Won't toy with the action queue just yet.
                    void $ readMVar hback -- Waits until hback is full.
                    currentState <- readMVar state -- This is to make sure that the old state will only be replaced once the action is carried out.
                    (commands, newstate) <- runStateT (action retro) currentState
                    void $ swapMVar state newstate
                    dequeueMVar actionQ
                    handleCommands commands
                    consumeForever vwaiting vresult vtransmitting bot_id threads refer actionQ hbseq hback quit connection state
                    where
                        handleCommands = void . sequence . map performCommand

                        performCommand command = case command of
                            SendPayload pl      -> sendJSON connection pl
                            Refer act           -> enqueueInMVar refer act
                            Enqueue act         -> enqueueInMVar actionQ (False, act)
                            Voice gid cid path  -> transmitVoice gid cid path
                            FakePayload pl      -> handlePayload actionQ hbseq hback quit connection pl
                            Fork xs             -> createThread threads $ handleCommands xs
                            Wait i              -> threadDelay i
                            ShutDown            -> void $ tryPutMVar quit False

                        transmitVoice gid cid filepath = do
                            alreadydoingso <- tryReadMVar vtransmitting                 -- Check if we're already trying to transmit voice through a MVar. If we are, we simply don't do anything.
                            unless (isJust alreadydoingso) $ void . forkIO $            -- We fork the lot, so the bot can do other stuff while transmitting voice.
                                flip catchIOError (\_ -> putStrLn ".dca file doesn't exist.") $ do
                                    handle <- openBinaryFile (filepath ++ ".dca") ReadMode      -- We need to open the .dca file with openBinaryFile, because openFile applies some weird formatting so it may be interpreted as text.
                                    hSetBuffering handle NoBuffering                            -- This prevents caching of soundbytes. I think. There was a weird bug that made sometimes made certain soundbytes play at the wrong command, and I think the handle buffering is to blame. I haven't encountered the bug since, so...
                                    flip catchIOError (\e -> putStrLn "Non-handle related exception when transmitting voice. Report this: " >> print e >> hClose handle) $ do
                                        void $ tryPutMVar vtransmitting ()                                              -- Indicate we're transmitting voice
                                        void $ tryPutMVar vwaiting (Nothing, Nothing)                                   -- Indicate to produceForever that we're waiting for the Voice State Update and Voice Server Update payloads
                                        sendJSON connection VoiceStateUpdate{channel_id = (Just cid), guild_id = gid}   -- Tell the gateway about our intentions to connect. It should respond with the aforementioned payloads.
                                        (session, token, sid, endpoint) <- takeMVar vresult                             -- Wait for the result from produceForever.
                                        let host = takeWhile (/=':') endpoint                                           -- the endpoint goes something like this: "some.host.com:80". The ":80" is irrelevant, so we remove it.
                                        runSecureClient host 443 "/" $ \conn -> do                                      -- Connect to the voice websocket.
                                            sendJSON conn $ VoiceIdentify sid bot_id session token                      -- Send a VoiceIdentify payload to the voice websocket.
                                            let until2 = do                                                             -- Get payloads from the websocket until we receive the op-code 2 payload. This is neccesary because there's a mystery OP-8 payload the voice websocket sends at this point that serves no purpose except to annoy you.
                                                    pl <- getJSON conn
                                                    case pl of
                                                        VoiceReady{}    -> return pl
                                                        _               -> until2

                                            VoiceReady ssrc' port interval <- until2                                    -- The ssrc as we receive it from the Ready payload is a number. We need to translate it into a ByteString.

                                            let sendHeartbeats = forever $ do                                           -- Start the hearbeat sender. We don't listen to ACKS, as we don't really care if we lose connection.
                                                    time <- epoch 
                                                    sendJSON conn $ VoiceHeartbeat time
                                                    threadDelay (interval * 1000)

                                            toKill <- forkIO sendHeartbeats
                                            let ssrc = Strict.concat . BS.toChunks . Bin.encode $ ssrc'

                                            (addr:_)    <- getAddrInfo Nothing (Just host) (Just port)                          -- Haha, time to establish the UDP connection! Using Network.Socket! I can't be arsed to explain this, it just works.
                                            vsocket     <- socket (addrFamily addr) Datagram defaultProtocol        
                                            NS.connect vsocket (addrAddress addr)      
                                            -- setSocketOption vsocket SendBuffer 65535                                            -- There's a default limit to how large packets we can send. We strip that away. Not actually neccesary.
    
                                            void $ send vsocket $ Strict.append ssrc $ Strict.replicate 66 0x00                 -- Send the 70-byte packet for IP-discovery over the UDP socket.
                                            ipdiscovery <- recv vsocket 70                                                      -- Wait for another 70-byte packet from the UDP socket. This will contain our IP and port.
                                            let ip = decodeUtf8 . (Strict.takeWhile (/=0x00)) . (Strict.drop 4) $ ipdiscovery   -- Our IP begins at index 4, and is null-terminated.
                                            let udpport = Bin.decode $ BS.fromChunks [Strict.cons (Strict.index ipdiscovery 69) $ Strict.singleton $ Strict.index ipdiscovery 68] -- The port is stored in the last two bytes, encoded in little endian. We translate it into a number.
                                            --print ip
                                            --print udpport
                                            sendJSON conn $ SelectProtocol ip udpport   -- Tell the voice websocket (not to be confused with the UDP socket) of our IP and port.
                                            let until4 = do
                                                    pl <- getJSON conn
                                                    case pl of
                                                        SessionDescription{}    -> return pl
                                                        _                       -> until4
                                            SessionDescription key <- until4            -- Get our encryption key from the session description payload that the voice websocket responds with.
                                            let staticpart = Strict.pack [0x80, 0x78]   -- Build the static part of all UDP packets. This will always be at the beginning of all UDP packets, no matter what.

                                            sendJSON conn $ Speaking True               -- Indicate that we're speaking.
                                            delay <- newEmptyMVar                       -- This MVar is responsible for keeping a consistent delay between each packet we send.
                                            let sendloop sq timestamp silence = do
                                                    forkIO $ void $ threadDelay 15000 >> tryPutMVar delay ()    -- There should be a very specific delay between packets sent. I haven't been able to figure out exactly what it is, however, 15000 ms seems to be very close to it. 
                                                    let
                                                        header  = Strict.concat [staticpart, Strict.concat (BS.toChunks sq), Strict.concat (BS.toChunks timestamp), ssrc] -- Construct the header.

                                                        nonce :: Nonce
                                                        nonce   = maybe (error "Failed to convert RTP Header to Nonce") id $ Salt.decode $ Strict.append header $ Strict.replicate 12 0x00
    
                                                        newsq           = Bin.encode $ 1 + (Bin.decode sq :: Word16)            -- Thanks to the way decode works, if we try to decode a bytestring larger than 16 bits, the upper bits are truncated. This gives us the desired behaviour that sq "wraps around" to 0 after FFFF.

                                                        newtimestamp    = Bin.encode $ 960 + (Bin.decode timestamp :: Word32)   -- Same thing with sq, timestamp wraps around to 0 after FFFF FFFF.

                                                    case silence of
                                                        Just i | i > 0  -> do
                                                            void $ send vsocket $ Strict.append header $ secretbox key nonce $ Strict.pack [0xF8, 0xFF, 0xFE]
                                                            void $ takeMVar delay
                                                            sendloop newsq newtimestamp (Just (i - 1))
                                                        Nothing         -> do
                                                            len <- BS.hGetNonBlocking handle 2
                                                            if BS.length len /= 2 then return $ Just (sq, timestamp)
                                                            else do
                                                                let
                                                                    packlength :: Int
                                                                    packlength = fromIntegral $ (Bin.decode $ BS.reverse len :: Word16) -- Determine the length of the upcoming voice packet in the .dca file. We reverse len, because the two bytes are actually in little endian, and we need them to be in big endian.
    
                                                                toSend <- Strict.hGetNonBlocking handle packlength
                                                                void $ send vsocket $ Strict.append header $ secretbox key nonce toSend
                                                                void $ takeMVar delay
                                                                sendloop newsq newtimestamp silence
                                                        _               -> return Nothing

                                            Just (sq, timestamp) <- sendloop (Bin.encode (0 :: Word16))  (Bin.encode (0 :: Word32)) Nothing
                                            sendloop sq timestamp (Just 5)          -- Send 5 frames of silence
                                            sendJSON conn $ Speaking False          -- Indicate we're not speaking
                                            killThread toKill                       -- Kill the heartbeat loop
                                            close vsocket                           -- Close the UDP socket
                                            sendClose conn ("" :: ByteString)       -- Close the voice websocket

                                        hClose handle
                                        sendJSON connection VoiceStateUpdate{channel_id = Nothing, guild_id = gid}  -- Disconnect from the voice channel
                                        void $ tryTakeMVar vtransmitting                                            -- Finally, indicate we're not transmitting anymore.


{- react behaviour eventName
   PURPOSE: Get a reaction function from behaviour and eventname
   PRE: True
   POST: Reaction defined in behaviour for the eventName 
-}
react :: Behaviour a -> String -> Reaction a
react Behaviour{reactions=ReactionSet{..}} eventName =
    case eventName of
        "CHANNEL_CREATE"            -> channelCreate
        "CHANNEL_UPDATE"            -> channelUpdate
        "CHANNEL_DELETE"            -> channelDelete
        "GUILD_CREATE"              -> guildCreate
        "GUILD_UPDATE"              -> guildUpdate
        "GUILD_DELETE"              -> guildDelete
        "GUILD_BAN_DELETE"          -> guildBanAdd
        "GUILD_BAN_REMOVE"          -> guildBanRemove
        "GUILD_EMOJIS_UPDATE"       -> guildEmojisUpdate
        "GUILD_INTEGRATIONS_UPDATE" -> guildIntegrationsUpdate
        "GUILD_MEMBER_ADD"          -> guildMemberAdd
        "GUILD_MEMBER_REMOVE"       -> guildMemberRemove
        "GUILD_MEMBER_UPDATE"       -> guildMemberUpdate
        "GUILD_MEMBERS_CHUNK"       -> guildMembersChunk
        "GUILD_ROLE_CREATE"         -> guildRoleCreate
        "GUILD_ROLE_UPDATE"         -> guildRoleUpdate
        "GUILD_ROLE_DELETE"         -> guildRoleDelete
        "MESSAGE_CREATE"            -> messageCreate
        "MESSAGE_UPDATE"            -> messageUpdate
        "MESSAGE_DELETE"            -> messageDelete
        "MESSAGE_DELETE_BULK"       -> messageDeleteBulk
        "PRESENCE_UPDATE"           -> presenceUpdate
        "GAME_OBJECT"               -> gameObject
        "TYPING_START"              -> typingStart
        "USER_SETTINGS_UPDATE"      -> userSettingsUpdate
        "USER_UPDATE"               -> userUpdate
        "VOICE_STATE_UPDATE"        -> voiceStateUpdate
--        "VOICE_SERVER_UPDATE"       -> voiceServerUpdate
        _ -> (\_ _ -> (liftIO $ putStrLn $ "Some strange eventName! :" ++ eventName) >> return [])


