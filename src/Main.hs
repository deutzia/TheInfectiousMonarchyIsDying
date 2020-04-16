module Main where

import Control.Monad.Except
import Data.Either
import Text.Megaparsec
import Text.Megaparsec.Char
import System.Environment
import Data.Void
import Control.Monad
import Parser
import Types
import Eval

exec :: Program -> Either String String
exec program = do
    env <- runExcept $ prepareEnv program
    results <- runExcept $ runProgram program env
    return $ concatMap printResult results

main = do
    argv <- getArgs
    contents <- if null argv then getContents else readFile $ head argv
    let name = if null argv then "stdin" else head argv
        parseResult = runParser pProgram name contents
      in
      case parseResult of
        Left error -> putStrLn $ errorBundlePretty error
        Right program -> let res = exec program
                         in
                         case res of
                            Left error -> putStrLn $ "ERROR: " ++ error
                            Right correct -> putStr correct

