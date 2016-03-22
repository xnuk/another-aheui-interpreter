module Jamo where

import Data.Char (ord, chr)

data Cho = {-!data-cho-} {-!data-deriving-}
data Jung = {-!data-jung-} {-!data-deriving-}
data Jong = {-!data-jong-} {-!data-deriving-}

type Moeum = Jung
type Batchim = Jong
data Syllable = Syllable Cho Moeum Batchim deriving (Eq, Ord)

syllable :: Char -> Maybe Syllable
syllable a
    | a<'가' || a>'힣' = Nothing
    | otherwise = let (y, jong) = (ord a-44032) `divMod` 28
                      (cho, jung) = y `divMod` 21
                  in Just $ Syllable (toEnum cho) (toEnum jung) (toEnum jong)

instance Show Syllable where
    show (Syllable cho jung jong) = (:"") . chr $ (fromEnum cho * 21 + fromEnum jung) * 28 + fromEnum jong + 44032

class (Eq a, Enum a) => Jaeum a where
    {-!jaeum-comma-sep-} :: a

instance Jaeum Cho where
{-!jaeum-cho-}

instance Jaeum Jong where
{-!jaeum-jong-}

{-!unique-cho-comma-sep-} :: Cho
{-!unique-cho-}

{-!unique-jong-comma-sep-} :: Jong
{-!unique-jong-}

{-!jung-comma-sep-} :: Jung
{-!jung-}
