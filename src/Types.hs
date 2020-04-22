{-# Options -Wall -Wname-shadowing #-}
module Types where

import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Text.PrettyPrint as PP

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

-- data Primitive = Primitive String Int ([Data] -> EvalM Data)
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

addToEnv :: Env -> String -> Data -> Env
addToEnv (rho, s, l) name d =
    (M.insert name l rho, M.insert l d s, l + 1)

{- typecheck written based on http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.65.7733&rep=rep1&type=pdf -}

data Type    =  TVariable String
             |  TInt
             |  TBool
             |  TFun Type Type
             deriving (Eq, Ord)

