{-# Options -Wall -Wname-shadowing #-}
module Main where

import Control.Monad.Except
import Control.Monad.State
import Text.Megaparsec
import System.Environment
import qualified Data.Map.Lazy as M
import Parser
import Types
import Eval

exec :: Program -> Either String String
exec program = do
    results <- evalState (runExceptT $ runProgram program) (M.empty, M.empty, 0)
    return $ concatMap printResult results

main :: IO ()
main = do
    argv <- getArgs
    contents <- if null argv then getContents else readFile $ head argv
    let name = if null argv then "stdin" else head argv
        parseResult = runParser pProgram name contents
      in
      case parseResult of
        Left err -> putStrLn $ errorBundlePretty err
        Right program -> let res = exec program
                         in
                         case res of
                            Left err -> putStrLn $ "ERROR: " ++ err
                            Right correct -> putStr correct

