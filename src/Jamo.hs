module Jamo where

import Data.Char (ord, chr)

data Cho = Cㄱ|Cㄲ|Cㄴ|Cㄷ|Cㄹ|Cㅁ|Cㅂ|Cㅅ|Cㅆ|Cㅇ|Cㅈ|Cㅊ|Cㅋ|Cㅌ|Cㅍ|Cㅎ|Cㄸ|Cㅃ|Cㅉ deriving (Enum, Eq, Ord, Show)
data Jung = Mㅏ|Mㅐ|Mㅑ|Mㅒ|Mㅓ|Mㅔ|Mㅕ|Mㅖ|Mㅗ|Mㅘ|Mㅙ|Mㅚ|Mㅛ|Mㅜ|Mㅝ|Mㅞ|Mㅟ|Mㅠ|Mㅡ|Mㅢ|Mㅣ deriving (Enum, Eq, Ord, Show)
data Jong = J_|Jㄱ|Jㄲ|Jㄳ|Jㄴ|Jㄵ|Jㄶ|Jㄷ|Jㄹ|Jㄺ|Jㄻ|Jㄼ|Jㄽ|Jㄾ|Jㄿ|Jㅀ|Jㅄ|Jㅁ|Jㅂ|Jㅅ|Jㅆ|Jㅇ|Jㅈ|Jㅊ|Jㅋ|Jㅌ|Jㅍ|Jㅎ deriving (Enum, Eq, Ord, Show)

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
    ㄱ,ㄲ,ㄴ,ㄷ,ㄹ,ㅁ,ㅂ,ㅅ,ㅆ,ㅇ,ㅈ,ㅊ,ㅋ,ㅌ,ㅍ,ㅎ :: a

instance Jaeum Cho where
    ㄱ = Cㄱ
    ㄲ = Cㄲ
    ㄴ = Cㄴ
    ㄷ = Cㄷ
    ㄹ = Cㄹ
    ㅁ = Cㅁ
    ㅂ = Cㅂ
    ㅅ = Cㅅ
    ㅆ = Cㅆ
    ㅇ = Cㅇ
    ㅈ = Cㅈ
    ㅊ = Cㅊ
    ㅋ = Cㅋ
    ㅌ = Cㅌ
    ㅍ = Cㅍ
    ㅎ = Cㅎ

instance Jaeum Jong where
    ㄱ = Jㄱ
    ㄲ = Jㄲ
    ㄴ = Jㄴ
    ㄷ = Jㄷ
    ㄹ = Jㄹ
    ㅁ = Jㅁ
    ㅂ = Jㅂ
    ㅅ = Jㅅ
    ㅆ = Jㅆ
    ㅇ = Jㅇ
    ㅈ = Jㅈ
    ㅊ = Jㅊ
    ㅋ = Jㅋ
    ㅌ = Jㅌ
    ㅍ = Jㅍ
    ㅎ = Jㅎ

ㄸ,ㅃ,ㅉ :: Cho
ㄸ = Cㄸ
ㅃ = Cㅃ
ㅉ = Cㅉ

ㄳ,ㄵ,ㄶ,ㄺ,ㄻ,ㄼ,ㄽ,ㄾ,ㄿ,ㅀ,ㅄ :: Jong
ㄳ = Jㄳ
ㄵ = Jㄵ
ㄶ = Jㄶ
ㄺ = Jㄺ
ㄻ = Jㄻ
ㄼ = Jㄼ
ㄽ = Jㄽ
ㄾ = Jㄾ
ㄿ = Jㄿ
ㅀ = Jㅀ
ㅄ = Jㅄ

ㅏ,ㅐ,ㅑ,ㅒ,ㅓ,ㅔ,ㅕ,ㅖ,ㅗ,ㅘ,ㅙ,ㅚ,ㅛ,ㅜ,ㅝ,ㅞ,ㅟ,ㅠ,ㅡ,ㅢ,ㅣ :: Jung
ㅏ = Mㅏ
ㅐ = Mㅐ
ㅑ = Mㅑ
ㅒ = Mㅒ
ㅓ = Mㅓ
ㅔ = Mㅔ
ㅕ = Mㅕ
ㅖ = Mㅖ
ㅗ = Mㅗ
ㅘ = Mㅘ
ㅙ = Mㅙ
ㅚ = Mㅚ
ㅛ = Mㅛ
ㅜ = Mㅜ
ㅝ = Mㅝ
ㅞ = Mㅞ
ㅟ = Mㅟ
ㅠ = Mㅠ
ㅡ = Mㅡ
ㅢ = Mㅢ
ㅣ = Mㅣ
