{-# LANGUAGE OverloadedStrings #-}

module Parser (codeParser, isSpace, isEndOfLine) where

import Prelude hiding (takeWhile, null)

-- import Data.Attoparsec.Text hiding (endOfLine, isEndOfLine)
import Data.Attoparsec.Text (Parser, parseOnly, inClass, takeWhile, takeWhile1, takeTill, skip, skipWhile, string, endOfInput, sepBy, choice, satisfy, many')
--import Data.Attoparsec.Combinator (lookAhead)
import Control.Applicative ((<|>))
import Control.Monad (void, liftM2)
import Data.Monoid ((<>))
import Data.Map.Strict (Map, fromList)
import Data.Char (generalCategory, GeneralCategory(Space))
import Data.Text (Text, unpack, null, singleton)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, isNothing)
import Data.List (dropWhileEnd)
import Data.Array (Array, array, listArray)
import Jamo (syllable, Syllable)
import Util (if')

isSpace :: Char -> Bool
isSpace x = x == '\x09'
         || x == '\x0B'
         || x == '\x0C'
         || x == '\x20'
         || x == '\xA0'
         || x == '\xFEFF'
         || generalCategory x == Space

isEndOfLine :: Char -> Bool
isEndOfLine x = x == '\n'
             || x == '\r'
             || x == '\x2028'
             || x == '\x2029'

endOfLine :: Parser ()
endOfLine = void (string "\r\n") <|> skip isEndOfLine

getEndOfLine :: Parser Text
getEndOfLine = string "\r\n" <|> singleton <$> satisfy isEndOfLine

langExtendFlag :: Parser (ByteString, ByteString)
langExtendFlag = do
    string "#:"
    skipWhile isSpace
    flag <- takeWhile1 isName
    skipWhile isSpace
    value <- takeWhile isName
    skipWhile isSpace
    _ <- getEndOfLine
    return (encodeUtf8 flag, if' (null value) "SET" (encodeUtf8 value))
  where isName = inClass "A-Z_"

endOfLangExtend :: Parser ()
endOfLangExtend = do
    takeTill $ \x -> inClass "가-힣" x || isEndOfLine x
    getEndOfLine
    return ()

langExtendParser :: Parser [Maybe (ByteString, ByteString)]
langExtendParser = many' $ choice [Just <$> langExtendFlag, const Nothing <$> endOfLangExtend]

instructionArea :: Parser [Maybe Syllable]
instructionArea = do
    xs <- takeTill isEndOfLine
    return . map syllable $ unpack xs

instructionAreaParser :: Parser [[Maybe Syllable]]
instructionAreaParser = choice [const [] <$> endOfInput, instructionArea] `sepBy` endOfLine

{-
codeParser :: Text -> Either String (Map ByteString ByteString, Array Int (Array Int (Maybe Syllable)))
codeParser x = case parse langExtendParser x of
                 Fail _ _ err -> Left err
                 Partial _ -> Left "Not enough input"
                 Done inp flags -> case parseOnly instructionAreaParser inp of
                                     Left err -> Left err
                                     Right xs -> Right (fromList (catMaybes flags), array (0, length arrs - 1) arrs)
                                         where trimedArea = dropWhileEnd (all isNothing) . dropWhile (all isNothing) $ xs
                                               arrs = zip [0..] $ map (\v -> listArray (0, length v - 1) v) trimedArea
-}

codeParser :: Text -> Either String (Map ByteString ByteString, Array Int (Array Int (Maybe Syllable)))
codeParser x = do
    (flags, xs) <- parseOnly (liftM2 (,) langExtendParser instructionAreaParser) (x <> "\n")
    let trimedArea = dropWhileEnd (all isNothing) . dropWhile (all isNothing) $ xs
        arrs = zip [0..] $ map (\v -> listArray (0, length v - 1) v) trimedArea
    Right (fromList (catMaybes flags), array (0, length arrs - 1) arrs)
    {-
      Because of using Ix Int, maximum rows/cols support is 2^29. (536,870,912).
      Otherwise, undefined behavior.
    -}
