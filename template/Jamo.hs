{-# LANGUAGE OverloadedStrings #-}
module Jamo where

import Prelude hiding (readFile, putStrLn, lookup, writeFile)
import Data.List (intersect, (\\))
import Data.Map (fromList, lookup)
import Data.Attoparsec.Text (takeWhile1, takeTill, inClass, char, parseOnly, choice, many', endOfInput)
import Data.Text (Text, cons, unpack, pack, singleton, intercalate, intersperse)
import Data.Text.IO (readFile, writeFile)
import qualified Data.Text as T
import Data.Monoid ((<>))
import Debug.Trace (trace)

cho = "ㄱㄲㄴㄷㄹㅁㅂㅅㅆㅇㅈㅊㅋㅌㅍㅎㄸㅃㅉ"
jong = "ㄱㄲㄳㄴㄵㄶㄷㄹㄺㄻㄼㄽㄾㄿㅀㅄㅁㅂㅅㅆㅇㅈㅊㅋㅌㅍㅎ"
jung = "ㅏㅐㅑㅒㅓㅔㅕㅖㅗㅘㅙㅚㅛㅜㅝㅞㅟㅠㅡㅢㅣ"
jaeum = intersect cho jong
unique_cho = cho \\ jaeum
unique_jong = jong \\ jaeum

declBody :: Char -> Bool -> String -> Text
declBody c indent = intercalate "\n" . map (\x -> pack ((if indent then "    " else "") ++ x:" = " ++ c:x:""))

datas = fromList [ ("data-cho",  intercalate "|" $ map (cons 'C' . singleton) cho)
                 , ("data-jung", intercalate "|" $ map (cons 'M' . singleton) jung)
                 , ("data-jong", intercalate "|" $ map (cons 'J' . singleton) ('_':jong))
                 , ("jaeum-comma-sep",       intersperse ',' $ pack jaeum)
                 , ("unique-cho-comma-sep",  intersperse ',' $ pack unique_cho)
                 , ("unique-jong-comma-sep", intersperse ',' $ pack unique_jong)
                 , ("jung-comma-sep",        intersperse ',' $ pack jung)
                 , ("jaeum-cho",  declBody 'C' True jaeum)
                 , ("jaeum-jong", declBody 'J' True jaeum)
                 , ("unique-cho",  declBody 'C' False unique_cho)
                 , ("unique-jong", declBody 'J' False unique_jong)
                 , ("jung",        declBody 'M' False jung)
                 , ("data-deriving", "deriving (Enum, Eq, Ord, Show)")
                 ]

holeParser = do
    "{-!"
    a <- takeWhile1 (inClass "a-z-")
    let b = T.last a == '-'
        k = if b then T.init a else a
    if b
       then "}"
       else "-}"
    case lookup k datas of
      Just z  -> return z
      Nothing -> error $ "Cannot find " ++ unpack k


takeallParser = takeWhile1 (/='{')
consumebraceParser = do
    char '{'
    a <- takeTill (=='{')
    return a


main :: IO ()
main = do
    a <- readFile "template/Jamo.template.hs"
    case parseOnly (many' $ choice [holeParser, consumebraceParser, takeallParser]) a of
      Left err -> error err
      Right xs -> writeFile "src/Jamo.hs" $ T.concat xs
