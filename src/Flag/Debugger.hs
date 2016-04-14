{-# LANGUAGE DeriveDataTypeable #-}
module Flag.Debugger (Debugger(..)) where

import Data.Data (Data)

data Debugger = UNSET | SET | ALL deriving (Show, Eq, Read, Ord, Bounded, Data)
