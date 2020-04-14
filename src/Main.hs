module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import Data.Void
import Control.Monad
import Parser
import Types
import Eval

main = do
    argv <- getArgs
    contents <- if null argv then getContents else readFile $ head argv
    let name = if null argv then "stdin" else head argv
        parseResult = runParser pProgram name contents
      in
      case parseResult of
        Left error -> putStrLn $ errorBundlePretty error
        Right program -> let env = prepareEnv program
                             results = runProgram program env
                             strResult = concatMap printResult results
                            in putStr strResult

