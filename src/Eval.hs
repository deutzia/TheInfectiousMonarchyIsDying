{-# Options -Wall -Wname-shadowing #-}
{-# Language FlexibleContexts #-}
module Eval where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Lazy as M
import Data.Maybe
import Types

nextLoc :: MonadState Env m => m Loc
nextLoc = state (\(rho, store, l) -> (l, (rho, store, l + 1)))

lookUpStore :: MonadState Env m => Loc -> m (Maybe Data)
lookUpStore loc = state (\(rho, store, l) -> (M.lookup loc store, (rho, store, l)))

addStore :: MonadState Env m => Loc -> Data -> m ()
addStore i d = state (\(rho, store, l) -> ((), (rho, M.insert i d store, l)))

lookUpRho :: MonadState Env m => String -> m (Maybe Loc)
lookUpRho name = state (\(rho, store, l) -> (M.lookup name rho, (rho, store, l)))

addRho :: MonadState Env m => String -> Loc -> m ()
addRho s i = state (\(rho, store, l) -> ((), (M.insert s i rho, store, l)))

prepareEnv :: Program -> EvalM ()
prepareEnv prog =
    let
        helperRho :: TopLevelExp -> (Loc, Rho) -> (Loc, Rho)
        helperRho (Expr _) env = env
        helperRho (Def n _) (l, rho) = (l + 1, M.insert n l rho)
        helperStore :: TopLevelExp -> (Rho, Store) -> (Rho, Store)
        helperStore (Expr _) env = env
        helperStore (Def n t) (rho, store) =
            let
                l = fromJust $ M.lookup n rho
            in (rho, M.insert l (DLazyEval t rho) store)
    in do
        (rho, store, l) <- get
        (newL, newRho) <- return $ foldr helperRho (l, rho) prog
        (_, newStore) <- return $ foldr helperStore (newRho, store) prog
        put (newRho, newStore, newL)

runProgram :: Program -> EvalM Result
runProgram prog =
    let
        helper :: Result -> TopLevelExp -> EvalM Result
        helper l (Def _ _) = return l
        helper l (Expr e) = do
            h <- eval e
            return $ h : l
    -- reverse here, because foldM is like foldl, not foldr
    in do
        prepareEnv prog
        lazyResults <- foldM helper [] $ reverse prog
        mapM unlazy lazyResults

eval :: AST -> EvalM Data
eval (AData d) = return d
eval (AVariable var) = do
    maybeRes <- lookUpRho var
    case maybeRes of
        Nothing -> fail $ "invalid identifier: " ++ var
        Just r -> return $ DReference r
eval (AFunApp fun arg) =
    do
        argval <- eval arg
        funval <- eval fun
        result <- nextLoc
        addStore result (DLazyApp funval argval)
        return $ DReference result
eval (ALambda name tree) = do
    (rho, _, _) <- get
    return $ DFun name tree rho
eval (ALet decls tree) =
    let
        helperRho :: (String, AST) -> (Loc, Rho) -> (Loc, Rho)
        helperRho (name, _) (l, rho) = (l + 1, M.insert name l rho)
        helperStore :: (String, AST) -> (Rho, Store) -> (Rho, Store)
        helperStore (name, t) (rho, store) =
            let
                l = fromJust $ M.lookup name rho
            in (rho, M.insert l (DLazyEval t rho) store)
    in do
        (rho, store, l) <- get
        (newL, newRho) <- return $ foldr helperRho (l, rho) decls
        (_, newStore) <- return $ foldr helperStore (newRho, store) decls
        put (rho, newStore, newL)
        l_ <- nextLoc
        addStore l_ $ DLazyEval tree newRho
        return $ DReference l

unlazy :: Data -> EvalM Data
unlazy (DLazyApp fun arg) = do
    funval <- unlazy fun
    case funval of
        DFun name tree fRho -> do
            l <- nextLoc
            addStore l arg
            (rho, store, l_) <- get
            put (M.insert name l fRho, store, l_)
            result <- unlazy =<< eval tree
            (_, newStore, newL) <- get
            put (rho, newStore, newL)
            return result
        DPrim (Primitive name n f) ->
            if n == 1
                then unlazy =<< f [arg]
                else return $ DPrim (Primitive name (n-1) (\l -> f (arg:l)))
        _ -> fail "trying to apply someting that is not a function"
unlazy (DLazyEval tree eRho) = do
    (rho, store, l) <- get
    put (eRho, store, l)
    result <- unlazy =<< eval tree
    (_, newStore, newL) <- get
    put (rho, newStore, newL)
    return result
unlazy (DReference loc) = do
    maybeData <- lookUpStore loc
    addStore loc DUndefined
    value <- case maybeData of
        Nothing -> fail "undefined reference"
        Just d -> return d
    result <- unlazy value
    addStore loc result
    return result
unlazy DUndefined = fail "unsolvable loop"
unlazy a = return a

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

