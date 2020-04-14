module Eval where

import qualified Data.Map.Lazy as M
import Data.Maybe
import Types

prepareEnv :: Program -> Env
prepareEnv prog =
    let
        helper :: TopLevelExp -> Env -> Env
        helper (Expr _) env = env
        helper (Def n t) env = M.insert n (eval t result) env
        result = foldr helper M.empty prog
    in result

runProgram :: Program -> Env -> Result
runProgram prog env =
    let
        helper :: TopLevelExp  -> [Data] -> [Data]
        helper (Def _ _) l = l
        helper (Expr e) l = eval e env : l
    in foldr helper [] prog

eval :: AST -> Env -> Data
eval (AData d) _ = d
eval (AVariable var) env = fromJust $ M.lookup var env
eval (AFunApp fun arg) env =
    let
        argval = eval arg env
        funval = eval fun env
    in
        case funval of
            DFun name tree fenv -> eval tree (M.insert name argval fenv)
            DPrim (Primitive name n fun) ->
                if n == 1
                    then fun [argval]
                    else DPrim (Primitive name (n-1) (\l -> fun (argval:l)))
            _ -> undefined
eval (ALambda name tree) env = DFun name tree env
eval (ALet l tree) env =
    let
        newenv = foldr (\(x, t) nenv -> M.insert x (eval t newenv) nenv) env l
    in eval tree newenv
eval (AIf cond e1 e2) env =
    let
        condval = eval cond env
        e1val = eval e1 env
        e2val = eval e2 env
    in
        case condval of
            DBool b -> if b then e1val else e2val
            _ -> undefined

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

