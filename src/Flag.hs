{-# LANGUAGE DeriveDataTypeable #-}

module Flag (WsIgnore(..), SetUnset(..), Spec(..), Flag(..), getFlag, isFlag, reMap) where

import Prelude hiding (lookup)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack, singleton)
import Data.Map.Strict (Map, foldrWithKey')
import Data.Set (Set, lookupGE, insert, empty, member)
import Data.Monoid ((<>))

import Data.Data (toConstr, Data)
import Text.Read (readMaybe)

import Util (if')
{-
data Flag a = Flag { name :: a
                   , canBe :: a -> Bool
                   , defValue :: a
                   }-}

data WsIgnore = ALL | WHITESPACE | LINE_BREAK | DO_NOT_IGNORE deriving (Show, Eq, Read, Ord, Bounded, Data)
data SetUnset = UNSET | SET deriving (Show, Eq, Read, Ord, Bounded, Data)
data Spec = NULL deriving (Show, Eq, Read, Ord, Bounded, Data)


data Flag = CONDITIONAL_CANNON_JUMP SetUnset
          | CANNON_JUMP SetUnset
          | NEED_MORE_COPY SetUnset
          | DEEP_EXCHANGE SetUnset
          | CALC_BY_BATCHIM SetUnset
          | OVERWRITE_REDEFINED_BATCHIMS SetUnset
          | INPUT_WHITESPACE_IGNORE_LEVEL WsIgnore
          | PASSAGE_SPEC Spec
          deriving (Show, Eq, Read, Ord, Data)
{-
setOrUnsetFlag :: ByteString -> Flag ByteString
setOrUnsetFlag x = Flag x (`elem` ["SET", "UNSET"]) "UNSET"

condCannonJump, cannonJump, moreCopy, deepExchange, calcBatchim, redefBatchim, inputWhitespaceIgnore, passageSpec :: Flag ByteString
condCannonJump = setOrUnsetFlag "CONDITIONAL_CANNON_JUMP"
cannonJump = setOrUnsetFlag "CANNON_JUMP"
moreCopy = setOrUnsetFlag "NEED_MORE_COPY"
deepExchange = setOrUnsetFlag "DEEP_EXCHANGE"
calcBatchim = setOrUnsetFlag "CALC_BY_BATCHIM"
redefBatchim = setOrUnsetFlag "OVERWRITE_REDEFINED_BATCHIMS"
inputWhitespaceIgnore = Flag "INPUT_WHITESPACE_IGNORE_LEVEL" (`elem` ["ALL", "WHITESPACE", "LINE_BREAK", "DO_NOT_IGNORE"]) "ALL"
passageSpec = Flag "PASSAGE_SPEC" (/= "SET") "NULL"

get :: Ord a => Flag a -> Map a a -> a
get Flag {defValue = def, name = k} = fromMaybe def . lookup k

set :: Ord a => Flag a -> a -> Map a a -> Map a a
set Flag {defValue = def, name = k, canBe = canbe} x = insert k (if' (canbe x) x def)

reMap :: Map ByteString ByteString -> Map ByteString ByteString
--reMap m = foldr f empty [condCannonJump, cannonJump, moreCopy, deepExchange, calcBatchim, redefBatchim, inputWhitespaceIgnore, passageSpec]
--    where f flag = set flag (get flag m)-}
reMap :: Map ByteString ByteString -> Set Flag
reMap = foldrWithKey' (\k a s -> maybe s (`insert` s) (readMaybe . unpack $ k <> singleton ' ' <> a)) empty

getFlag :: Bounded a => (a -> Flag) -> Set Flag -> Flag
getFlag f s = maybe z (\x -> if' (toConstr x == toConstr z) x z) $ lookupGE z s
    where z = f minBound

isFlag :: (a -> Flag) -> a -> Set Flag -> Bool
isFlag f a = member (f a)

-- fromConstrB (fromConstr (toConstr x)) (toConstr a)

-- foldr :: (a -> b -> b) -> b -> [a] -> b
