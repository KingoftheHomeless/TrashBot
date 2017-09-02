{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
import Bot
import State hiding (lookup)
import qualified State as ST
import Control.Monad
import Control.Monad.Except
import Data.Aeson.Types hiding (Options,Result)
import Data.Char (toLower)
import Data.HashMap.Lazy as Map (HashMap, foldlWithKey', toList,(!),member,elems,empty, keys)
import Network.Wreq hiding (delete, Payload, asValue)
import Data.Text (Text,unpack,pack,isInfixOf)
import qualified Data.Text as Txt
import Data.Functor (void)
import Text.Read (readMaybe)
import Control.Monad.State (liftIO)
import Prelude hiding (lookup)
import Text.Read (readMaybe)
import qualified Data.List as List (isInfixOf)

-- You must provide the authorization token for the bot you registered, by replacing the string that token is bound to.
token = "INSERT_TOKEN_HERE"
main = launchBot token testBehaviour

---------------------------------
-- THE PROVIDED TEST BEHAVIOUR --
---------------------------------

data TrashError = NetworkError BadRequest | StateError Exception deriving (Show)

instance SupportsNetworkError TrashError where
    fromBR = NetworkError
    toBR (NetworkError x)   = Just x
    toBR _                  = Nothing

instance StateValue Permissions

instance BotStateInstance State where
    empty = State.empty

instance BotStateInstanceDeletable State [Text] where
    deleter keys = \st -> case alterTraverseE st (\_ -> Nothing) keys False of
        (Right x)           -> x
        _                   -> st

instance StateValue a => BotStateInstanceSearchable TrashError State [Text] a where
    fetcher keys = \st -> case lookupTraverse st keys of
        (Right x)   -> Right x
        (Left x)    -> Left . StateError $ x

    inserter keys val = \st -> case alterTraverseE st (\_ -> Just $ toState val) keys True of
        (Right x)   -> Right x
        (Left x)    -> Left . StateError $ x


testBehaviour :: Behaviour State
testBehaviour = defaultBehaviour {
        reactions = testReactions,
        onTimer = sendAutomaticMessage,
        timer = 2                    -- For test case 6; determines how often sendAutomaticMessage is run.
    }

testReactions :: ReactionSet State
testReactions = defaultReactions {
        ready = initialize,
        resumed = initialize,
        messageCreate = hMessageCreate, -- For test cases 1, 2, 3, 4, and 6
        channelCreate = hChannelCreate, -- For test case 5
        voiceStateUpdate = hVoiceStateUpdate,
        guildCreate = hGuildCreate

    }

-- An alias for memoize, except it always adds the key "memo" in the keylist.
memo :: StateValue v => [Text] -> ActionMonad TrashError State v -> ActionMonad TrashError State v
memo [] _           = error "Empty keylist not allowed."
memo key question   = memoize ("memo" : key) question

-- getChannelMemberPermissions' provided with memoize functions for State
getPermissions :: String -> String -> ActionMonad TrashError State Permissions
getPermissions cid uid = getChannelMemberPermissions' (memo ["channels", pack cid, "object"]) (\gid -> memo ["guilds", pack gid, "object"]) (\gid -> memo ["guilds", pack gid, "users", pack uid, "object"]) cid uid

initialize :: Reaction State
initialize _ = status1

status1 :: Action State
status1 False   = return [SendPayload $ Status $ "ENDLESS TRASH", Fork [Wait 300000000, Enqueue status2]]   -- No skipError neccesary, we don't actually need to use the ActionMonad
status1 _       = return []

status2 :: Action State
status2 False   = return [SendPayload $ Status $ "a jape", Fork [Wait 300000000, Enqueue status3]]
status2 _       = return []

status3 :: Action State
status3 False   = return [SendPayload $ Status $ "Supreme Leader", Fork [Wait 300000000, Enqueue status1]]
status3 _       = return []


------------------------------------------------
-- HANDLER FUNCTION FOR MESSAGE_CREATE EVENTS --
------------------------------------------------
{- hMessageCreate eventData
    PURPOSE: Handle the event MESSAGE_CREATE, i.e. a message being sent to a channel.
             Passes the event to another function, which will perform an (re)action.

    PRE: eventData is from an authentic event from the discord gateway.

    POST: a list of commands for the bot to execute that can't be executed in the ActionMonad. Depends on the function the event is passed to.

    SIDE EFFECTS: 
        All side-effects of the function that the event is passed to.
        hMessageCreate also prints the message as given by eventData to standard output (usually the command prompt/terminal).
-}
hMessageCreate :: Reaction State
hMessageCreate eD retro = 
    let 
        ci = (filterJSON eD "channel_id")
        cont = (filterJSON eD "content") :: String
        author = (filterJSON eD "author")
        username = filterJSON author "username" :: String
        isbot = lookupJSON author "bot" :: Maybe Bool
    in
        skipError $ do
            channel <- memo ["channels", pack ci, "object"] $ wait $ getChannel ci
            if (filterJSON channel "type" /= (0 :: Int)) then
                return []
            else do
                let channel_name    = filterJSON channel "name"
                let guild_id        = filterJSON channel "guild_id"
                guild_name          <- fmap (flip filterJSON "name") $ memo (["guilds", pack guild_id, "object"]) $ wait $ getGuild guild_id
                liftIO $ putStrLn ("%" ++ guild_name ++ "% #" ++ channel_name ++ " <" ++ username ++ ">: " ++ cont)
            
                if null cont || maybe False id isbot then -- Usually, the contents of a message are nonempty. But if it isn't, then simply return state. TrashBot also doesn't react to bots.
                    return []
                else
                    if head cont == '!'
                        then let com:args = Prelude.words cont in handleCommand com args
                        else censor (map toLower cont) eD retro    -- If the message isn't a command, pass it the function for test case 3
                                                                   -- together with the contents of the message
    where
        {- handleCommand command commandargs
            PURPOSE: Calls appropiate function to handle a command in chat (e.g. "!prune").
                     These functions will be of type ([String] -> Reaction a), where [String] is the list of arguments to the command.

                     Some commands aren't meant to take any arguments.
                     Functions for these may be written to be of type Reaction a. If they are, then
                     anonymous functions are needed to convert these to type ([String] -> Reaction a),
                     where the first argument is ignored.
            PRE: True
            POST: a list of commands for the bot to execute that can't be executed in the ActionMonad. Depends on the function the event is passed to.
            SIDE EFFECTS:
                All side-effects of the function handleCommand calls.
        -}
        handleCommand :: String -> [String] -> ActionMonad TrashError State [Command State]
        handleCommand comm args = f args eD retro
            where
                f = case map toLower comm of 
                    "!whoareyou"                -> \_ -> identify -- Test case 1. The command doesn't take any arguments.
                    "!prune"                    -> prune          -- Test case 2.
                    "!store"                    -> store          -- Test case 4.
                    "!retrieve"                 -> retrieve        -- Test case 4.
                    "!spam"                     -> toggleAutomaticMessages -- Test case 6. The command doesn't take any arguments.
                    "!delete"                   -> deletecomm
                    "!shutdown"                 -> \_ -> shutdown
                    "!quit"                     -> \_ -> shutdown
                    "!trash"                    -> \_ -> transmitvoice "TRASH"
                    "!flush"                    -> \_ -> flush
                    "!reconnect"                -> \_ _ _ -> return [FakePayload Reconnect]
                    "!status"                   -> \args _ _ -> return [SendPayload $ Status $ unwords args]        --TODO: Restrict to those with Admin permissions.
                    c                           -> \_ _ _ -> (liftIO . putStrLn) ("Unknown command: " ++ c) >> return []


-- Deletes memoization storage. You need to flush in case you have memoized functions that return volatile information, and you know there's a risk that the information has changed.
flush :: Reaction' TrashError State
flush eD _ =
    let
        author = filterJSON eD "author"

        cid :: String
        cid = filterJSON eD "channel_id"

        uid :: String
        uid = filterJSON author "id"
    in do
        permissions <- memo ["channels", pack cid, "users", pack uid, "permissions"] $ getPermissions cid uid
        when (isAdmin permissions) $ delete ["memo" :: Text] >> (liftIO . putStrLn) "Flushed memo storage"
        return []

transmitvoice :: FilePath -> Reaction' TrashError State
transmitvoice filepath eD False =
    let
        author = filterJSON eD "author"

        ci :: String
        ci = filterJSON eD "channel_id"

        uid :: Text
        uid = filterJSON author "id"
    in do
        channel <- memo ["channels", pack ci, "object"] $ wait $ getChannel ci
        let
            gid :: String
            gid = filterJSON channel "guild_id"
        vstate <- fetch ["guilds", pack gid, "users", uid, "voice state"]
        return [Voice gid vstate filepath]
transmitvoice _ _ _             = return []


hGuildCreate :: Reaction State
hGuildCreate eD _ = 
    let
        gid :: Text
        gid     = filterJSON eD "id"

        vstates :: [Value]
        vstates = filterJSON eD "voice_states"
    in skipError $ do
            try $ do        -- In case we've saved previous information about users voice state, delete the lot.
                orig <- fmap keys $ (fetch ["guilds", gid, "users"] :: ActionMonad TrashError State (HashMap Text Value))
                void . sequence $ fmap (\k -> delete ["guilds", gid, "users", k, "voice state"]) orig
            void . sequence $
                [
                insert ["guilds", gid, "users", filterJSON v "user_id", "voice state"] cid
                    | 
                        v <- vstates,
                        let cid = lookupJSON v "channel_id",
                        case cid of {Just (String _) -> True; _ -> False}
                ]
            insert ["memo", "guilds", gid, "object"] eD
            return []


hVoiceStateUpdate :: Reaction State
hVoiceStateUpdate eD _ = 
    let
        gid :: Text
        gid = filterJSON eD "guild_id"


        cid :: Value
        cid = filterJSON eD "channel_id"


        uid :: Text
        uid = filterJSON eD "user_id"
    in skipError $ insert ["guilds", gid, "users", uid, "voice state"] cid >> return []

{- identify eventData
    PURPOSE: Performs the action for the command "!whoareyou".
    PRE: eventData is from an authentic event from the discord gateway
    POST: []
    SIDE-EFFECTS: Responds within the channel that the command was written in with the message "I'm the Trash Bot! I EAT GARBAGE!"
-}
identify :: Reaction' TrashError State
identify eD retro = (unless retro $ try . asValue $ createMessage (filterJSON eD "channel_id") "I'm the Trash Bot! I EAT GARBAGE!" False) >> return [] -- Here, we use try, as it's not really important that the message goes through. We don't even wait for ratelimits.


-- shutdown shuts the bot down unless the action is performed as part of replaying events.
shutdown :: Reaction' TrashError State 
shutdown _ True     = return []
shutdown eD _       = 
    let
        cid :: String
        cid = filterJSON eD "channel_id"

        uid :: String
        uid = filterJSON (filterJSON eD "author") "id"
    in do
        permissions <- memo ["channels", pack cid, "users", pack uid, "permissions"] $ wait $ getPermissions cid uid
        if isAdmin permissions
            then (try . asValue $ createMessage (filterJSON eD "channel_id") "GOODBYE CRUEL WORLD!" False) >> return [ShutDown, Wait 1000000] -- Wait is to prevent the bot from consuming its own "GOODBYE CRUEL WORLD!" message, which, when printed, blends with the "Shutting down... message"
            else (liftIO . putStrLn) "Unauthorized (needs to be admin)" >> return []


{- prune args eventData
   PURPOSE: Performs the action for the command "!prune" (with an argument)
            delete a set number of messages from the channel of eventData,
            depending on the first argument in args (all other are ignored)

   PRE: eventData is data of an MESSAGE_CREATE event within a channel which authHeader grants access to

   POST: empty list.
   SIDE-EFFECTS:
                If the !prune command was written by someone with manageMessages permissions, and
                if args contains a number in first position: send a request to the discord API to bulk-delete
                that number of last messages received in the channel the !prune commad was written.
                otherwise no side effects
-}
prune :: [String] -> Reaction' TrashError State
prune [] _ _ = return []
prune args eD retro
    | retro = return []
    | otherwise =
        let
            cid :: String
            cid = filterJSON eD "channel_id"

            uid :: String
            uid = filterJSON (filterJSON eD "author") "id"
        in do
            permissions <- memo ["channels", pack cid, "users", pack uid, "permissions"] $ wait $ getPermissions cid uid
            if permissions `hasPermission` manageMessages
                then
                    case readMaybe (args !! 0) of
                        (Just limit) | limit >= 1 && limit < 99 -> do
                            let ci = filterJSON eD "channel_id"
                            mtry $ do
                                history <- wait $ getChannelMessages ci (limit + 1)
                                let x = Prelude.map (\obj -> filterJSON obj "id") history :: [String]
                                catchError (void . asValue $ wait $ bulkDeleteMessages ci x) (\err -> (liftIO (putStrLn $ "Bad Request when deleting: " ++ show err)) >> return ())
                                return []
                        _ -> (liftIO $ putStrLn $ "Invalid args for prune: " ++ show args) >> return []
                else
                    (liftIO . putStrLn) "Unauthorized (needs to be able to manage messages)" >> return []


{- censor content eventData
   PURPOSE: Reads content (which is the contents of a message) for offensive phrases.
            If the corresponding message is found inflammatory, then that message is deleted,
            and the bot responds with a reprimand. 
            If the corresponding message is not found inflammatory, then the bot does nothing.
   PRE: eventData is data of a MESSAGE_CREATE event within a channel which authHeader grants access to.

   POST: empty list.
   SIDE-EFFECTS: 
        If the phrases "bad bot", "horrible bot", or "clean bot" is part of content,
        then delete the message that eventData represents.
        Responds with "THAT IS NOT NICE." in the channel that the message was created.
        Won't do anything if the reaction is performed as part of replaying events.
-}
censor :: String -> Reaction' TrashError State
censor cont eD retro
    | insulting = delete >> unless retro (try . asValue $ wait $ createMessage ci "THAT IS NOT NICE." False) >> return []
    | otherwise = return []
    where
        insulting = any (`List.isInfixOf` cont) ["bad bot", "horrible bot", "clean bot"]
        ci = (filterJSON eD "channel_id")
        messageid = (filterJSON eD "id")
        delete = catchError (void . asValue $ wait $ deleteMessage ci messageid) (\err -> (liftIO (putStrLn $ "Bad Request when deleting: " ++ show err)) >> return ())

{- store commandargs eventData authorizationHeader state
    PURPOSE: Performs the action for the command "!store" (with multiple arguments)
             Stores a string within the bot's state to a key, that can later be retrieved using "!retrieve"

    PRE: eventData is data of an Message Create event within a channel which authHeader grants access to.

    POST:
        Otherwise, return state, unmodified.
    SIDE-EFFECTS:
        If length of commandargs is greater than 1:
            Consider the first command argument a key. 
            Then, concatenate all other command arguments to a string with a space seperating them. Consider this the value.
            Update state, except within an internal map exclusive to the server within which the command was made,
            the state now stores a key-value pairing, mapping the key to the value.
            Performs a GET HTTP request to obtain extra information unless that information has been memoized.
            Responds with "Data stored." in the channel that the command was made.
        Otherwise, none.

-}
store :: [String] -> Reaction' TrashError State
store [] eD _       = return []
store (arg:content) eD retro
    | null content  = return []
    | otherwise     =
        let 
            ci = (filterJSON eD "channel_id")
        in do
            channel <- memo ["channels", pack ci, "object"] $ getChannel ci
            unless retro $ try . asValue $ createMessage ci "Data stored." False
            insert ["guilds", (filterJSON channel "guild_id"), "storage", pack arg] (unwords content)
            return []


--Deletes a key in storage, and says as much.
deletecomm :: [String] -> Reaction' TrashError State
deletecomm [] eD _          = return []
deletecomm (arg:_) eD retro =
    let 
        ci = (filterJSON eD "channel_id")
    in do
        channel <- memo ["channels", pack ci, "object"] $ getChannel ci
        delete ["guilds", (filterJSON channel "guild_id"), "storage", pack arg]
        unless retro $ try . asValue $ createMessage ci "Key deleted if found." False
        return []


{- retrieve commandargs eventData authorizationHeader state
    PURPOSE: Performs the action for the command "!retrieve" (with multiple arguments)
             Retrieves a string bound to a key, that was previously stored using the command "!store"

    PRE: eventData is data of a MESSAGE_CREATE event within a channel which authHeader grants access to.

    POST: State, unmodified.
    SIDE-EFFECTS:
        If commandargs is not empty, consider the first string in commandargs to be the key.
        If the state contains a value bound to the key, for the server that the command was made,
        then respond with that value in quotation marks, in the channel that the command was made.
        Otherwise, responds with "Key not found." in the channel that the command was made.
-}
retrieve :: [String] -> Reaction' TrashError State
retrieve [] eD _        = return []
retrieve _ _ True       = return []
retrieve (arg:_) eD _   =
    let 
        ci = (filterJSON eD "channel_id")
    in do
        channel <- memo ["channels", pack ci, "object"] $ getChannel ci
        catchError 
            (do
                stored <- fetch ["guilds", (filterJSON channel "guild_id"), "storage", pack arg]
                try . asValue $ createMessage ci ("\"" ++ stored ++ "\"") False
            )
            (\_ -> try . asValue $ createMessage ci "Key not found." False)
        return []


{- hChannelCreate eventData authorizationHeader state
    PURPOSE: Sends a message to a newly created text channel.

    PRE: eventData is data of an CHANNEL_CREATE event, and authHeader grants access to the channel in question.

    POST: empty list.
    SIDE-EFFECTS:
        Sends a welcoming message in the channel indicated by eventData.
-}
hChannelCreate :: Reaction State
hChannelCreate eD _
    | textChannel = skipError $ (asRequest $ wait (createMessage channelId ("Welcome to the world, " ++ channelName ++ "! Enjoy your stay.") False)) >> return []
    | otherwise = return []
    where
        textChannel = (filterJSON eD "type" :: Int) == 0
        channelId = filterJSON eD "id"
        channelName = filterJSON eD "name"

{-
    toggleAutomaticMessages will toggle automatic messages for the channel where an admininstrator has given the command "!spam"
    The bot will spam the channel with a message every two seconds. With text-to-speech.
    If no arguments are provided, the bot will spam the channel with "I'M ANNOYING."
    Otherwise, it will spam with the concatenation of the arguments, with spaces seperating them.
-}
toggleAutomaticMessages :: [String] -> Reaction' TrashError State
toggleAutomaticMessages args eD _ = 
    let
        message :: String
        message =
            case unwords args of
                ""  -> "I'M ANNOYING"
                x   -> x
        uid :: String
        uid = filterJSON (filterJSON eD "author") "id"

        ci :: String
        ci = filterJSON eD "channel_id"

        change (Just _) = Nothing
        change Nothing = Just $ String $ pack message
    in do
        permissions <- memo ["channels", pack ci, "users", pack uid, "permissions"] $ wait $ getPermissions ci uid
        if not (isAdmin permissions)
        then (liftIO . putStrLn) "Unauthorized (needs to be Admin)" >> return []
        else do
            channel <- memo ["channels", pack ci, "object"] $ wait $ getChannel ci
            update $ \st -> alterTraverse st change ["guilds", (filterJSON channel "guild_id"), "channels", pack ci, "AutomaticMessages"] True
            return []



{- sendAutomaticMessage
    PURPOSE: Intended to be called periodically. Sends a message to all channels where automatic messages are enabled.
             As timer in testBehaviour is 2, and onTimer in testBehaviour is mapped to this function,
             this action will be queued every 2 seconds.
    PRE: authHeader grants access to all channels for which automatic messages are enabled.
    POST: empty list.
    SIDE-EFFECTS:
        Determined by reading state:
            For every channel for which automatic messages are enabled
            (indicated by the having the key "AutomaticMessages" exist, and mapped to a message):
                send the message provided by state to that channel.

            No messages are sent to channels for which automatic messages are disabled.
-}
sendAutomaticMessage :: Action State
sendAutomaticMessage retro = skipError $ flip (>>) (return []) $ unless retro $ try $ do
    guilds <- fetch (["guilds"] :: [Text]) :: ActionMonad TrashError State (HashMap Text Value)
    void $ sequence $ findChannels (Map.elems guilds)
    where 
        findChannels :: [State] -> [ActionMonad TrashError State ()]
        findChannels glds = Prelude.foldr (\h t -> spam h : t) [] (Prelude.foldr (\st t -> Map.toList (findChannels' st) ++ t) [] glds)
            where
                findChannels' :: State -> HashMap Text State
                findChannels' st = case ST.lookup st "channels" of
                    Right (Object x) -> x
                    Left _ -> Map.empty

                spam :: (Text, State) -> ActionMonad TrashError State ()
                spam (key, obj) = case ST.lookup obj "AutomaticMessages" of
                    Right message -> try . asValue $ createMessage (unpack key) message True -- We REALLY don't want to use "wait" here. If we get rate limited, so be it.
                    _ -> return ()
