module Main where

import Prelude hiding (readFile)
import Interpret (interpret)
import Data.Text.IO (readFile)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= readFile . head >>= interpret
