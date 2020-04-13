module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import Data.Void
import Control.Monad

type Parser = Parsec Void String

main = do
    argv <- getArgs
    contents <- if null argv then getContents else readFile $ head argv
    putStrLn contents
