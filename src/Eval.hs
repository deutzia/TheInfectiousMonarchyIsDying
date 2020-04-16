module Eval where

import Control.Monad.Except
import qualified Data.Map.Lazy as M
import Data.Maybe
import Types

prepareEnv :: Program -> Except String Env
prepareEnv prog =
    let
        helper :: Env -> TopLevelExp -> Except String Env
        helper env (Expr _) = return env
        helper env (Def n t) = do
            result_ <- result
            toInsert <- eval t result_
            return $ M.insert n toInsert env
        result = foldM helper M.empty prog
    in result

runProgram :: Program -> Env -> Except String Result
runProgram prog env =
    let
        helper :: Result -> TopLevelExp -> Except String Result
        helper l (Def _ _) = return l
        helper l (Expr e) = do
            h <- eval e env
            return $ h : l
    -- reverse here, because foldM is like foldl, not foldr
    in foldM helper [] $ reverse prog

eval :: AST -> Env -> Except String Data
eval (AData d) _ = return d
eval (AVariable var) env =
    let
        maybeRes = M.lookup var env
    in
        case maybeRes of
            Nothing -> fail $ "invalid identifier: " ++ var
            Just r -> return r
eval (AFunApp fun arg) env =
    do
        argval <- eval arg env
        funval <- eval fun env
        case funval of
            DFun name tree fenv -> eval tree (M.insert name argval fenv)
            DPrim (Primitive name n fun) ->
                if n == 1
                    then return $ fun [argval]
                    else return $DPrim (Primitive name (n-1) (\l -> fun (argval:l)))
            _ -> fail $ "trying to apply someting that is not a function"
eval (ALambda name tree) env = return $ DFun name tree env
eval (ALet l tree) env =
    let
        helper :: Env -> (String, AST) -> Except String Env
--      helper nenv (x, t) = M.insert x (eval t newenv) nenv
        helper nenv (x, t) = do
            newenv_ <- newenv
            toInsert <- eval t newenv_
            return $ M.insert x toInsert nenv
        newenv = foldM helper env l
    in
        do
            newenv_ <- newenv
            eval tree newenv_

{- | Basic eval tests
>>> eval ( AData $ DInt 5 ) M.empty
DInt 5
>>> eval ( AData $ DBool True ) M.empty
DBool True
>>> eval ( ALet [("x", AData $ DInt 2), ("y", AData $ DInt 3)] ( AFunApp ( AFunApp (AData $ DPrim builtinAdd) (AVariable "x") ) (AVariable "y"))) M.empty
DInt 5
>>> eval (AFunApp (AData (DFun "x" ( AFunApp ( AFunApp (AData $ DPrim builtinMul) (AVariable "x") ) (AVariable "x")) M.empty)) (AData $ DInt 5)) M.empty
DInt 25
>>> eval ( AIf (AData $ DBool True) (AData $ DInt 5) (AData $ DInt 1)) M.empty
DInt 5
>>> eval ( AIf (AData $ DBool False) (AData $ DInt 5) (AData $ DInt 1)) M.empty
DInt 1
-}

