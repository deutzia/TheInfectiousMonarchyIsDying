module Types where

import qualified Data.Map.Lazy as M
import Data.Maybe

type Env = M.Map String Data
data Data = DInt Integer | DBool Bool | DFun String AST Env | DPrim Primitive

instance Show Data where
    show (DInt n) = "DInt " ++ show n
    show (DBool b) = "DBool " ++ show b
    show (DFun s t e) = "DFun " ++ show s ++ show t ++ show e
    show (DPrim ( Primitive name _ _)) = "DPrim " ++  name

data Primitive = Primitive String Int ([Data] -> Data)

-- abstract syntax tree
data AST
    = AData Data
    | AVariable String
    | AFunApp AST AST
    | ALambda String AST
    | ALet [(String, AST)] AST
    | AIf AST AST AST
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
printResult (DFun s t e) = "function\n"
printResult (DPrim (Primitive name _ _)) = name ++ "\n"

