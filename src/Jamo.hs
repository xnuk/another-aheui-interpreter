module Jamo(Cho, Jung, Jong(J_), Moeum, Batchim, Syllable(Syllable), syllable, Jaeum, ㄱ, ㄲ, ㄴ, ㄷ, ㄹ, ㅁ, ㅂ, ㅅ, ㅆ, ㅇ, ㅈ, ㅊ, ㅋ, ㅌ, ㅍ, ㅎ, ㄸ, ㅃ, ㅉ, ㄳ, ㄵ, ㄶ, ㄺ, ㄻ, ㄼ, ㄽ, ㄾ, ㄿ, ㅀ, ㅄ, ㅏ, ㅐ, ㅑ, ㅒ, ㅓ, ㅔ, ㅕ, ㅖ, ㅗ, ㅘ, ㅙ, ㅚ, ㅛ, ㅜ, ㅝ, ㅞ, ㅟ, ㅠ, ㅡ, ㅢ, ㅣ) where

import Data.Char (ord, chr)

data Cho = C_G|C_GG|C_N|C_D|C_DD|C_L|C_M|C_B|C_BB|C_S|C_SS|C_|C_J|C_JJ|C_CH|C_K|C_T|C_P|C_H deriving (Enum, Eq, Ord, Show)
data Jung = A|AE|YA|YAE|EO|E|YEO|YE|O|WA|WAE|OI|YO|U|WEO|WE|WI|YU|EU|UI|I deriving (Enum, Eq, Ord, Show)
data Jong = J_|J_G|J_GG|J_GS|J_N|J_NJ|J_NH|J_D|J_L|J_LG|J_LM|J_LB|J_LS|J_LT|J_LP|J_LH|J_M|J_B|J_BS|J_S|J_SS|J_NG|J_J|J_CH|J_K|J_T|J_P|J_H deriving (Enum, Eq, Ord, Show)

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
    ㄱ, ㄲ, ㄴ, ㄷ, ㄹ, ㅁ, ㅂ, ㅅ, ㅆ, ㅇ, ㅈ, ㅊ, ㅋ, ㅌ, ㅍ, ㅎ :: a
instance Jaeum Cho where
    ㄱ = C_G
    ㄲ = C_GG
    ㄴ = C_N
    ㄷ = C_D
    ㄹ = C_L
    ㅁ = C_M
    ㅂ = C_B
    ㅅ = C_S
    ㅆ = C_SS
    ㅇ = C_
    ㅈ = C_J
    ㅊ = C_CH
    ㅋ = C_K
    ㅌ = C_T
    ㅍ = C_P
    ㅎ = C_H
instance Jaeum Jong where
    ㄱ = J_G
    ㄲ = J_GG
    ㄴ = J_N
    ㄷ = J_D
    ㄹ = J_L
    ㅁ = J_M
    ㅂ = J_B
    ㅅ = J_S
    ㅆ = J_SS
    ㅇ = J_NG
    ㅈ = J_J
    ㅊ = J_CH
    ㅋ = J_K
    ㅌ = J_T
    ㅍ = J_P
    ㅎ = J_H

ㄸ, ㅃ, ㅉ :: Cho
ㄸ = C_DD
ㅃ = C_BB
ㅉ = C_JJ

ㄳ, ㄵ, ㄶ, ㄺ, ㄻ, ㄼ, ㄽ, ㄾ, ㄿ, ㅀ, ㅄ :: Jong
ㄳ = J_GS
ㄵ = J_NJ
ㄶ = J_NH
ㄺ = J_LG
ㄻ = J_LM
ㄼ = J_LB
ㄽ = J_LS
ㄾ = J_LT
ㄿ = J_LP
ㅀ = J_LH
ㅄ = J_BS

ㅏ, ㅐ, ㅑ, ㅒ, ㅓ, ㅔ, ㅕ, ㅖ, ㅗ, ㅘ, ㅙ, ㅚ, ㅛ, ㅜ, ㅝ, ㅞ, ㅟ, ㅠ, ㅡ, ㅢ, ㅣ :: Jung
ㅏ = A
ㅐ = AE
ㅑ = YA
ㅒ = YAE
ㅓ = EO
ㅔ = E
ㅕ = YEO
ㅖ = YE
ㅗ = O
ㅘ = WA
ㅙ = WAE
ㅚ = OI
ㅛ = YO
ㅜ = U
ㅝ = WEO
ㅞ = WE
ㅟ = WI
ㅠ = YU
ㅡ = EU
ㅢ = UI
ㅣ = I
