module Main where

import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import Data.Void
import Control.Monad
import Parser
import Types

main = do
    argv <- getArgs
    contents <- if null argv then getContents else readFile $ head argv
    result <-
        let
            name = if null argv then "stdin" else head argv
            parseResult = runParser pProgram name contents
        in case parseResult of
            Left error -> putStrLn $ errorBundlePretty error
            Right program ->
                let
                    env = prepareEnv program
                    results = runProgram program env
                    strResult = concat $ map printResult results
                in putStr strResult
    return result
