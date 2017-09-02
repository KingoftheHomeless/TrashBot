{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

-- Implements a datatype similar to JSON, mapping strings to several types of data
module State(Exception(BadKey,NonObject),Exceptional,State,StateValue,fromState,toState,decode,encode,empty,stObj,(&=),lookup,filter,lookupTraverse,filterTraverse,alter,alterE,alterTraverse,alterTraverseE,alterMassTraverse,alterMassTraverseE) where

import qualified Data.HashMap.Lazy as Map
import Data.Aeson.Types
import qualified Data.Aeson as Aeson
import Data.Vector (Vector)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Prelude hiding (lookup,filter)

data Exception = BadKey [Text] | NonObject [Text] State deriving (Eq, Show)


type Exceptional = Either Exception

throw :: Exception -> Exceptional a
throw = Left

exceptional :: (Exception -> a) -> (a -> a) -> Exceptional a -> a
exceptional exception _ (Left a) = exception a
exceptional _ f (Right a) = f a

addKey :: Exception -> Text -> Exception
addKey (BadKey ks) k = BadKey (k:ks)
addKey (NonObject ks st) k = NonObject (k:ks) st


{-  REPRESENTATION CONVENTION:
    State represents the memory/collective state of a program. It is an alias for the Value constructor, from Data.Aeson.Types.
    The data type for Value is as follows:
    data Value
        = Object !Object                            -- "Object" is an alias for HashMap Text Value.
        | Array !Array                              -- "Array" is an alias for Vector, from Data.Vector; fittingly so, as a Vector is an array.
        | String !Text                              -- "Text" is a data type from Data.Text. Optimized, storage-efficient strings, as opposed to character lists.
        | Number !Scientific                        -- "Scientific" is a data type from Data.Scientific. Fractional number type that is arbitrarily large and precise.
        | Bool !Bool                                -- Bool is Bool.
        | Null                                      -- Null is a constructor that this module does not make any use for. To support compatibility with Aeson, 
                                                    this module considers "Null" to be an empty object.

        Value is a tagged union, where the primary value type is "Object", which is an alias for (Hashmap Text Value).
        This module will use the term "object" to denote such a HashMap, wrapped in the Object constructor.
        A program intending to store a State should always use the Object constructor; all other constructors are simply meant to be values fetchable
        from the Object using a Text key. Note that other Objects are also potential values; Objects within Objects are part of what makes Value
        a good data type to use for States.
        The other reason why State isn't its own independent data type is that information that progams that make use of JSON
        want to store are often in the form of JSON.
        The fact that Aeson's data type for JSON is also be used for States, without the need of a taxing convertion process, is beneficial
        to the program's performance, and lessens the complexity of development.

        The Object that a program uses to keep track of its very "state" is refered to as a "state object". 
        All other values of the type State (including other Objects) are called "state values".
        Null is always considered to be an empty object.

    REPRESENTATION INVARIANT:
        A state value is always an element of an object.
        A state object is never an element of an object.
-}
type State = Value


{-
    The type class StateValue is an alias for the FromJSON and ToJSON type classes that Aeson defines.
    A StateValue is a value that may be converted to and from a state. It features an alias for toJSON.

    fromState is not strictly an alias for fromJSON. fromJSON returns a monad that can be used for exception handling; however, normal use of fromJSON
    should never fail as long as the programmer type-annotates correctly. fromState assumes this, and instead returns a pure value.
-}
class (FromJSON a, ToJSON a) => StateValue a where

    {- toState 
    PRE: True
    POST: a, converted to a state value, or potentially a state object.
    EXAMPLES:
        toState True == Bool True
        toState (1 :: Int) == Number 1.0
        toState (1 :: Double) == Number 1.0
        toState (Bool True) == Bool True
    -}
    toState :: a -> State
    toState = toJSON

    {- fromState st
    PRE: st may be transformed to the desired type.
         (Type annotation is needed to indicate desired type.)
    POST: a, where a is st transformed to the desired type.
    EXAMPLES:
        (fromState (Bool True) :: Bool) == True
        (fromState (Number 1.0) :: Int) == 1
        (fromState (Number 1.0) :: Double) == 1.0
        (fromState (Bool True) :: State) == Bool True
    -}
    fromState :: State -> a
    fromState = fromSuccess . fromJSON -- fromJSON gives potential for exception handling, but as long as you type annotate properly, fromJSON should never return error.
        where
            --fromSuccess: Extracts a value wrapped in the Result monad. Raises an error if input is Error.
            fromSuccess :: Result a -> a
            fromSuccess (Success a) = a
            fromSuccess (Error s) = error $ "State.fromState: Type annotation missing or is faulty: " ++ s

-- These instances are for the data types used in Aeson's "value" data type.


instance StateValue Value
instance StateValue a => StateValue (Map.HashMap Text a)
instance StateValue a => StateValue (Vector a)
instance StateValue Text
instance StateValue Scientific
instance StateValue Bool

--These instances are for data types more commonly used. These are not exhaustive; there are many more FromJSON and ToJSON instances.
instance StateValue a => StateValue [a]
instance StateValue a => StateValue (Maybe a)
instance StateValue Char
instance StateValue Int
instance StateValue Integer
instance StateValue Float
instance StateValue Double

-- The "void" state value or state object; an empty object. It's better to use this, rather than Null.
empty :: State
empty = emptyObject


encode :: StateValue a => a -> ByteString
encode = Aeson.encode

decode :: StateValue a => ByteString -> Maybe a
decode = Aeson.decode

{- stObj pairs
PURPOSE: Alias for Aeson's object
PRE: True
POST: An object that contains the following, for each tuple in pairs:
    the first element of that tuple, mapped to the second element of that tuple.
-}
stObj :: [(Text, State)] -> State
stObj = object


{- key &= val
    PURPOSE: Synctatic sugar for creating a key-value pair, for use with stObj. 
             &= is the equivalent of Aeson's (.=). Close to an alias, but 
             has the Constraint of StateValue v instead of toJSON v, and will always return a tuple.
    PRE: True
    POST: (key, st), where st is val transformed to a state value.
-}
(&=) :: StateValue v => Text -> v -> (Text, Value)
(&=) key val = (key, toState val)

{- lookup st key
    PRE: True
    POST:
        If st is an object, and key is a valid key for an element within that object:
            return the value that key is bound to.
        Otherwise, return an exception.
-}
lookup :: StateValue a => State -> Text -> Exceptional a
lookup (Object m) key = maybe (throw $ BadKey [key]) (return . fromState) (Map.lookup key m)
lookup Null key = throw $ BadKey [key]
lookup st _ = throw $ NonObject [] st

{- filter st key
    PURPOSE: variant of lookup that returns a pure value instead of wrapping it in the Either monad.
             Raises an error when lookup would return an exception.
    PRE: st is an object, key is a valid key for an element within that object.
    POST: return the value that key is bound to.
-}
filter :: StateValue a => State -> Text -> a
filter st key = exceptional (\x -> error $ "State.filter: " ++ show x) id (lookup st key)


{- lookupTraverse st keylist
    PURPOSE: lookupTraverse "traverses" internal objects within st according to the keylist, before returning whatever is bound to the final key.
             It's postcondition is intuitive, but the accurate description is rather obtuse.
    PRE: True

    POST: 
        If keylist is empty, or st is not an object, return an exception.
        Otherwise:
            Consider the first key in keylist to be a key within st.
            For each key k in keylist:
                If k is the last key in keylist:
                    return the element that key is bound to.
                    If k is not bound, return an exception.
                
                Otherwise:
                    if k is bound to an object, consider the next key in keylist to be a key within that object.
                    if k is not bound, or not bound to an object, return an exception.
    VARIANT: |keys|
-}
lookupTraverse :: StateValue a => State -> [Text] -> Exceptional a
lookupTraverse st [] = throw $ BadKey []
lookupTraverse st [k] = lookup st k
lookupTraverse st@(Object m) (k:ks) = 
    case lookup st k of
        (Right m) ->
            case lookupTraverse m ks of
                (Left x) -> throw $ addKey x k
                result -> result
        (Left x) -> throw $ x
lookupTraverse st _ = throw $ NonObject [] st

{- filterTraverse st keys
    PURPOSE: variant of lookupTraverse that returns a pure object instead of wrapping it in the Either monad.
             Raises an error when lookup would return an exception.
    PRE: 
        • st is a Object. The head of keys is a key to an element in st. 
        • For each key k but the head in keys:
            • the key before k is bound to an Object that is an element for some Object.
            • k is bound to some element that is within the Object that the key before it is bound to.

    POST: the element that the last key in keys is bound to (within the Object that the key before it is bound to.)
    VARIANT: |keys|
-}
filterTraverse :: StateValue a => State -> [Text] -> a
filterTraverse st keys = exceptional (\x -> error $ "State.filterTraverse: " ++ show x) id (lookupTraverse st keys)


{- alterE st func key
    PURPOSE: Very flexible function for altering a single value within an object; may be used to insert, modify, and delete state values.
    PRE: True

    POST:
        If st is not an object, return an exception.
        Otherwise:
            • if key is bound to an element x in st: 
                let "result" be (func (Just x))
              otherwise
                let "result" be (func Nothing)

            • Then, if result is Nothing:
                return st, but without the element that key is bounded to (if any)
              if result is (Just newval):
                return st, replacing the element that key is bound to with newval
-}
alterE :: State -> (Maybe State -> Maybe State) -> Text -> Exceptional State
alterE (Object m) f key = return . toState $ Map.alter f key m
alterE Null f key = return . toState $ Map.alter f key (Map.empty)
alterE st _ _ = throw $ NonObject [] st


{- alter st func key
    PRE: st is an object.

    POST:
        • if key is bound to an element x in st: 
            let "result" be (func (Just x))
          otherwise
            let "result" be (func Nothing)

        • Then, if result is Nothing:
            return st, but without the element that key is bounded to (if any)
          Otherwise, if result is (Just newval):
            return st, replacing the element that key is bound to with newval
-}
alter :: State -> (Maybe State -> Maybe State) -> Text -> State
alter st f key = exceptional (\x -> error $ "State.alter: " ++ show x) id (alterE st f key)



{- alterTraverseE st func keylist insert
    PURPOSE: alterTraverseE "traverses" internal objects within st according to the keylist,
             before altering whatever is bound to the final key.
    PRE: True
    POST:
        If keylist is empty, or st is not an object, return an exception.
        Otherwise:
            • Consider the first key in keylist to be a key within st.
            • For each key k in keylist:
                If k is the last key in keylist:
                    Return st, but alter what k is bound to by the following rules:
                        • if k is bound to an element x: 
                            let "result" be (func (Just x))
                          otherwise
                            let "result" be (func Nothing)
            
                        • Then, if result is Nothing:
                            don't bind k to anything (unbind if previously binded)
                          Otherwise, if result is (Just newval):
                            bind k to newval

                Otherwise:
                    If k is bound to an object, consider the next key in keylist to be a key within that object.
                    Otherwise, if k is not bound to an object, and insert is True, bind k to an empty object, and consider the next key in the keylist to be a key within that object.
                    Otherwise,return an exception.
    VARIANT: |keylist|
-}
alterTraverseE :: State -> (Maybe State -> Maybe State) -> [Text] -> Bool -> Exceptional State
alterTraverseE _ _ [] _ = throw $ BadKey []
alterTraverseE st f [k] _ = return $ alter st f k
alterTraverseE Null f (k:ks) False = throw $ BadKey [k]
alterTraverseE Null f (k:ks) True = (alterTraverseE empty f ks True) >>= (\result -> return . toState $ Map.insert k result Map.empty)
alterTraverseE st@(Object m) f (k:ks) insert = 
        case (lookup st k) of
            (Right x) -> alterTraverseE' x
            _ | insert -> alterTraverseE' empty
            (Left x) -> throw $ x
    where
        alterTraverseE' nextstate = 
            case alterTraverseE nextstate f ks insert of
                (Left x) -> throw $ addKey x k
                (Right result) -> return . toState $ Map.insert k result m
alterTraverseE st _ _ _ = throw $ NonObject [] st


{- alterTraverse st func keylist insert
    PURPOSE: variant of alterTraverseE that returns a pure object instead of wrapping it in the Either monad.
             Raises an error when alterTraverseE would return an exception.
    PRE:  For each key k in keylist:
            One of the following applies:
                • k is bound to an object. If so, consider the next key in keylist to be a key within that object.
                • k is not bound to an object, and insert is True. If so, consider the next key in keylist to be a key within an empty object.
    POST: st, but alter what the last key in keylist is bound to by the following rules:
        • if the key is bound to an element x: 
            let "result" be (func (Just x))
          otherwise
            let "result" be (func Nothing)
        
        • Then, if result is Nothing:
            don't bind the key to anything (unbind if previously binded)
          Otherwise, if result is (Just newval):
            bind the key to newval
    VARIANT: |keylist|
-}
alterTraverse :: State -> (Maybe State -> Maybe State) -> [Text] -> Bool -> State
alterTraverse st f keys insert = exceptional (\x -> error $ "State.alterTraverse: " ++ show x) id (alterTraverseE st f keys insert)


{- alterMassTraverseE st keylists funcs insert
    PURPOSE: recursively applies alterTraverseE, where each keylist in keylists correspond to the func with the same index in funcs.
    PRE: True

    POST: 
        • If either keylists or funcs is empty, return st.
          Otherwise, 
                • Consider st to be the state for the first keylist in keylists corresponding to the first func in funcs.
                • for each keylist in keylists, corresponding to the func with the same index in funcs:
                    If keylist is the last keylist in keylist, or func is the last function in funcs:
                      return (alterTraverseE st keylist func insert)
                    Otherwise:
                      If (alterTraverseE state keylist func insert) returns an exception, return that exception.
                      Otherwise, consider (alterTraverseE state keylist func insert) to be the state for the next keylist in keylists, 
                      corresponding to the next func in funcs.
    VARIANT: |keylist| * |funcs|
-}
alterMassTraverseE :: State -> [(Maybe State -> Maybe State)] -> [[Text]] -> Bool -> Exceptional State
alterMassTraverseE st (f:fs) (p:ps) insert = (alterTraverseE st f p insert) >>= (\state -> alterMassTraverseE state fs ps insert)
alterMassTraverseE st _ _ _ = return st


{- alterMassTraverse st keylists funcs insert
    PURPOSE: variant of alterMassTraverseE that returns a pure object instead of wrapping it in the Either monad.
             Raises an error when alterTraverseE would return an exception.
    PRE:
        • Consider st to be the state for the first keylist in keylists corresponding to the first func in funcs.
          For each keylist in keylists but the last one, each corresponding to the func with the same index in funcs:  
            • state is a Object.
            • Consider the first key in keylist to be a key within state
            • For each keylist in keylists:
                For each key k in keylist:
                            One of the following applies:
                                • k is bound to an object. 
                                    If so, consider the next key in keylist to be a key within that object.
                                • k is not bound to an object, and insert is True.
                                    If so, consider the next key in keylist to be a key within an empty object.
                            • For each keylist in keylists:
                                Consider (alterTraverse state keylist func insert) to be the state for the next keylist in keylists
    POST:
        (alterTraverse state keylist func insert), where state is the state for the last keylist in keylists.
    VARIANT: |keylist| * |funcs|

-}
alterMassTraverse :: State -> [(Maybe State -> Maybe State)] -> [[Text]] -> Bool -> State
alterMassTraverse st fs ps insert = exceptional (\x -> error $ "State.alterTraverse: " ++ show x) id (alterMassTraverseE st fs ps insert)
