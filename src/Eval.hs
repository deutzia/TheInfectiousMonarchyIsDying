{-# Options -Wall -Wname-shadowing #-}
{-# Language FlexibleContexts #-}
module Eval where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
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
        helperRho (Algebraic _ _ prims (Primitive matchName _ _ _)) (l, rho) = foldl
            (\(l', rho') (Primitive name _ _ _) -> (l' + 1, M.insert name l' rho'))
            (l + 1, M.insert matchName l rho)
            prims
        helperStore :: TopLevelExp -> (Rho, Store) -> (Rho, Store)
        helperStore (Expr _) env = env
        helperStore (Def n t) (rho, store) =
            let
                l = fromJust $ M.lookup n rho
            in (rho, M.insert l (DLazyEval t rho) store)
        helperStore (Algebraic _ _ prims match@(Primitive matchName _ _ _)) (rho, store) =
            let
                matchL = fromJust $ M.lookup matchName rho
            in
                foldl
                    (\(rho', store') p@(Primitive name _ _ _) ->
                    let
                        l = fromJust $ M.lookup name rho'
                    in (rho', M.insert l (DPrim p) store')
                )
                (rho, M.insert matchL (DPrim match) store)
                prims
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
        helper l (Algebraic _ _ _ _) = return l
    in do
        let res = evalState (runReaderT (runExceptT $ typeCheck prog) TIEnv) 0
        case res of
            Left err -> fail $ "typecheck failed: " ++ err
            Right _ -> do
                prepareEnv prog
                -- reverse here, because foldM is like foldl, not foldr
                lazyResults <- foldM helper [] $ reverse prog
                mapM unlazy_full lazyResults

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
        return $ DReference l_
eval _ = undefined -- match is not passed to eval

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
        DPrim (Primitive name (TFun _ t) n f) ->
            if n == 1
                then unlazy =<< f [arg]
                else return $ DPrim (Primitive name t (n-1) (\l -> f (arg:l)))
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
unlazy (DPrim (Primitive _ _ 0 f)) = f []
unlazy a = return a

unlazy_full :: Data -> EvalM Data
unlazy_full a = do
    d <- unlazy a
    case d of
        DAlgebraic name elems -> do
            unlaziedElems <- mapM unlazy_full elems
            return $ DAlgebraic name unlaziedElems
        res -> return res
