{-# LANGUAGE TupleSections #-}
module PassageNull(PassageNull, newPassageNull) where

import PurePassage

data PassageNull = PassageNull deriving (Eq, Show)

instance PurePassage PassageNull where
    pop n = Just . (replicate n 0,)
    push _ = id
    actDup _ = id
    actSwap _ = id

newPassageNull :: PassageNull
newPassageNull = PassageNull
