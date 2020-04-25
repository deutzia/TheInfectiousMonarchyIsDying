{-# Options -Wall -Wname-shadowing #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import qualified Data.Graph as G
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe

type Loc = Int
type Rho = M.Map String Loc
type Store = M.Map Loc Data
type Env = (Rho, Store, Loc)
type EvalM = ExceptT String (State Env)

data Data
    = DInt Integer
    | DBool Bool
    | DFun String AST Rho
    | DPrim Primitive
    | DLazyApp Data Data
    | DLazyEval AST Rho
    | DReference Loc
    | DUndefined

instance Show Data where
    show (DInt n) = "DInt " ++ show n
    show (DBool b) = "DBool " ++ show b
    show (DFun s t e) = "DFun " ++ show s ++ show t ++ show e
    show (DPrim ( Primitive name _ n _)) = "DPrim (" ++  name ++ " " ++ show n ++ ")"
    show (DLazyApp d1 d2) = "DLazyApp " ++ show d1 ++ " " ++ show d2
    show (DLazyEval tree rho) = "DLazyEval " ++ show tree ++ " " ++ show rho
    show (DReference loc) = "DReference " ++ show loc
    show DUndefined = "DUndefined"

data Primitive = Primitive String Type Int ([Data] -> EvalM Data)

-- abstract syntax tree
data AST
    = AData Data
    | AVariable String
    | AFunApp AST AST
    | ALambda String AST
    | ALet [(String, AST)] AST
    deriving Show

data TopLevelExp
    = Def String AST
    | Expr AST
    deriving Show

type Program = [TopLevelExp]
type Result = [Data]

printResult :: Data -> String
printResult (DInt n) = show n ++ "\n"
printResult (DBool b) = show b ++ "\n"
printResult (DFun _ _ _) = "function\n"
printResult (DPrim (Primitive name _ _ _)) = name ++ "\n"
printResult (DLazyApp _ _) = "LazyApp\n"
printResult (DLazyEval _ _) = "LazyEval\n"
printResult (DReference n) = "Reference " ++ show n  ++ "\n"
printResult DUndefined = "Undefined\n"

{- typecheck written based on http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf -}

data Type
    = TVariable String
    | TInt
    | TBool
    | TFun Type Type
    deriving (Eq, Ord, Show)

-- order clauses in let so that ones that don't depend on the others are
-- ressolved first
-- for whatever reason strongly connected components are already implemented
-- https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Graph.html#g:7
-- so I may actually finish this on time
orderLet :: AST -> AST
orderLet (ALet l tree) =
    let
        vars = map fst l
        graph = map (\(name, t) -> ((name, t), name, filter (\v -> v `S.member` getVars t) vars)) l
        scc = map G.flattenSCC $ G.stronglyConnComp graph
    in foldr ALet tree scc
orderLet _ = undefined

getVars :: AST -> S.Set String
getVars (AData _) = S.empty
getVars (AVariable name) = S.singleton name
getVars (AFunApp t1 t2) = getVars t1 `S.union` getVars t2
getVars (ALambda name t) = name `S.delete` getVars t
getVars (ALet l tree) =
    let
        treeVars = getVars tree
        vars = S.fromList $ map fst l
        expVars = S.unions $ map (\(_, t) -> getVars t) l
    in (treeVars `S.union` expVars) `S.difference` vars

data Scheme  =  Scheme [String] Type

class Types a where
    -- free type variables
    ftv :: a -> S.Set String
    apply :: Subst -> a -> a

instance Types Type where
    ftv (TVariable n) = S.singleton n
    ftv TInt = S.empty
    ftv TBool = S.empty
    ftv (TFun t1 t2) = ftv t1 `S.union` ftv t2

    apply s (TVariable n) = fromMaybe (TVariable n) (M.lookup n s)
    apply s (TFun t1 t2) = TFun (apply s t1) (apply s t2)
    apply _ t = t

instance Types Scheme where
    ftv (Scheme vars t) = ftv t `S.difference` S.fromList vars

    apply s (Scheme vars t) = Scheme vars (apply (foldr M.delete s vars) t)

instance Types a => Types [a] where
    apply s  =  map (apply s)
    ftv = foldr (S.union . ftv ) S.empty

type Subst = M.Map String Type

nullSubst  ::  Subst
nullSubst  =   M.empty

compose         :: Subst -> Subst -> Subst
compose s1 s2   = M.map (apply s1) s2 `M.union` s1

type TypeEnv = M.Map String Scheme

remove :: TypeEnv -> String -> TypeEnv
remove env var  =  M.delete var env

instance Types TypeEnv where
    ftv env = ftv (M.elems env)
    apply s = M.map (apply s)

generalize :: TypeEnv -> Type -> Scheme
generalize env t =
    let
        vars = S.toList $ ftv t `S.difference` ftv env
    in Scheme vars t

data TIEnv = TIEnv {}
type TypeM = ExceptT String (ReaderT TIEnv (State Int))

newTyVar :: String -> TypeM Type
newTyVar prefix = do
    s <- state (\n -> (n, n + 1))
    return $ TVariable $ prefix ++ show s

instantiate :: Scheme -> TypeM Type
instantiate (Scheme vars t) = do
    nvars <- mapM (\ _ -> newTyVar "a") vars
    let s = M.fromList (zip vars nvars)
    return $ apply s t

-- return substituton needed to unify two particular types
mgu :: Type -> Type -> TypeM Subst
mgu (TFun l r) (TFun l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    return (s1 `compose` s2)
mgu (TVariable u) t = varBind u t
mgu t (TVariable u) = varBind u t
mgu TInt TInt = return nullSubst
mgu TBool TBool = return nullSubst
mgu t1 t2 = fail $ "types do not unify: " ++ show t1 ++ " vs. " ++ show t2

varBind :: String -> Type -> TypeM Subst
varBind u t | t == TVariable u = return nullSubst
            | u `S.member` ftv t = fail $ "occurs check fails: " ++ u ++ " vs. " ++ show t
            | otherwise = return (M.singleton u t)

-- type inference implementation
ti :: TypeEnv -> AST -> TypeM (Subst, Type)
ti _ (AData l) = case l of
    DInt _ -> return (nullSubst, TInt)
    DBool _ -> return (nullSubst, TBool)
    DPrim (Primitive _ t _ _) -> return (nullSubst, t)
    _ -> undefined
ti env (AVariable n) =
    case M.lookup n env of
        Nothing -> fail $ "unbound variable: " ++ n
        Just sigma -> do
            t <- instantiate sigma
            return (nullSubst, t)
ti env (AFunApp e1 e2) = do
    tv <- newTyVar "a"
    (s1, t1) <- ti env e1
    (s2, t2) <- ti (apply s1 env) e2
--    traceM $ "e1 = " ++ show e1 ++ " e2 = " ++ show e2
    s3 <- mgu (apply s2 t1) (TFun t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)
ti env (ALambda n e) = do
    tv <- newTyVar "a"
    let env' = M.insert n (Scheme [] tv) env
    (s1, t1) <- ti env' e
    return (s1, TFun (apply s1 tv) t1)
ti env let_@(ALet _ _) = do
    let actualLet = orderLet let_
    case actualLet of
        ALet l tree -> do
            (new, sub) <- helperLetEnv env l
            (sx, tx) <- ti new tree
            return (sx `compose` sub, tx)
        _ -> undefined

-- prepare env and susbstitutions needed for parsing let-clauses and for
-- preparing type environment (since global definitions are like a bunch of
-- let clauses
helperLetEnv :: TypeEnv -> [(String, AST)] -> TypeM (TypeEnv, Subst)
helperLetEnv env l =
    let
        helperInsert :: TypeEnv -> (String, AST) -> TypeM TypeEnv
        helperInsert e (n, _) = do
            tv <- newTyVar "a"
            return $ M.insert n (Scheme [] tv) e
        helperInfer :: (Subst, TypeEnv) -> (String, AST) -> TypeM (Subst, TypeEnv)
        helperInfer (sub, e) (n, tree) = do
            (s, t) <- ti e tree
            let newEnv = apply s e
            tv <- instantiate $ fromJust $ M.lookup n newEnv
            s_ <- mgu t tv
            return (s_ `compose` s `compose` sub, apply s_ newEnv)
        helperGeneralize :: (TypeEnv, TypeEnv) -> (String, AST) -> TypeM (TypeEnv, TypeEnv)
        helperGeneralize (acc, e) (n, _) = do
            tv <- instantiate $ fromJust $ M.lookup n acc
            let gen_t = generalize e tv
            return (M.insert n gen_t acc, e)
    in do
        new <- foldM helperInsert env l
        (sub, new') <- foldM helperInfer (nullSubst, new) l
        let env' = apply sub env
        (new'', _) <- foldM helperGeneralize (new', env') l
        return (new'', sub)

getClauses :: Program -> [(String, AST)]
getClauses =
    let
        helper :: TopLevelExp -> [(String, AST)] -> [(String, AST)]
        helper (Expr _) l = l
        helper (Def n t) l = (n, t) : l
    in foldr helper []

typeCheck :: Program -> TypeM ()
typeCheck prog =
    let
        clauses = getClauses prog
        helper :: TypeEnv -> TopLevelExp -> TypeM TypeEnv
        helper e (Def _ _) = return e
        helper e (Expr tree) = do
            (_, _) <- ti e tree
            return e
    in
        do
            (env, sub) <- helperLetEnv M.empty clauses
            foldM_ helper (apply sub env) prog

