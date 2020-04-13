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
    deriving Show

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

{- | Basic eval tests
>>> eval ( AData $ DInt 5 ) M.empty
DInt 5
>>> eval ( AData $ DBool True ) M.empty
DBool True
>>> eval ( ALet [("x", AData $ DInt 2), ("y", AData $ DInt 3)] ( AFunApp ( AFunApp (AData $ DPrim builtinAdd) (AVariable "x") ) (AVariable "y"))) M.empty
DInt 5
>>> eval (AFunApp (AData (DFun "x" ( AFunApp ( AFunApp (AData $ DPrim builtinMul) (AVariable "x") ) (AVariable "x")) M.empty)) (AData $ DInt 5)) M.empty
DInt 25
-}

builtinAdd :: Primitive
builtinAdd =
    let
        helper (DInt n : [DInt m]) = DInt (n + m)
        helper _ = undefined
    in Primitive "builtinAdd" 2 helper

builtinSub :: Primitive
builtinSub =
    let
        helper (DInt n : [DInt m]) = DInt (n - m)
        helper _ = undefined
    in Primitive "builtinSub" 2 helper

builtinMul :: Primitive
builtinMul =
    let
        helper (DInt n : [DInt m]) = DInt (n * m)
        helper _ = undefined
    in Primitive "builtinMul" 2 helper

builtinDiv :: Primitive
builtinDiv =
    let
        helper (DInt n : [DInt m]) = DInt (n `div` m)
        helper _ = undefined
    in Primitive "builtinDiv" 2 helper

builtinMod :: Primitive
builtinMod =
    let
        helper (DInt n : [DInt m]) = DInt (n `mod` m)
        helper _ = undefined
    in Primitive "builtinMod" 2 helper

builtinLand :: Primitive
builtinLand =
    let
        helper (DBool b1 : [DBool b2]) = DBool (b1 && b2)
        helper _ = undefined
    in Primitive "builtinLand" 2 helper

builtinLor :: Primitive
builtinLor =
    let
        helper (DBool b1 : [DBool b2]) = DBool (b1 || b2)
        helper _ = undefined
    in Primitive "builtinLor" 2 helper

builtinNeg :: Primitive
builtinNeg =
    let
        helper [DBool b] = DBool (not b)
        helper _ = undefined
    in Primitive "builtinNeg" 1 helper

builtinEq :: Primitive
builtinEq =
    let
        helper (DInt n : [DInt m]) = DBool (n == m)
        helper _ = undefined
    in Primitive "builtinEq" 2 helper

builtinNeq :: Primitive
builtinNeq =
    let
        helper (DInt n : [DInt m]) = DBool (n /= m)
        helper _ = undefined
    in Primitive "builtinNeq" 2 helper

builtinLt :: Primitive
builtinLt =
    let
        helper (DInt n : [DInt m]) = DBool (n < m)
        helper _ = undefined
    in Primitive "builtinLt" 2 helper

builtinLe :: Primitive
builtinLe =
    let
        helper (DInt n : [DInt m]) = DBool (n <= m)
        helper _ = undefined
    in Primitive "builtinLe" 2 helper

builtinGt :: Primitive
builtinGt =
    let
        helper (DInt n : [DInt m]) = DBool (n > m)
        helper _ = undefined
    in Primitive "builtinGt" 2 helper

builtinGe :: Primitive
builtinGe =
    let
        helper (DInt n : [DInt m]) = DBool (n >= m)
        helper _ = undefined
    in Primitive "builtinGe" 2 helper

{- | Primitives
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinAdd) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DInt 4
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinSub) (AData $ DInt 5) ) (AData $ DInt 2)) M.empty
DInt 3
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinMul) (AData $ DInt 5) ) (AData $ DInt 2)) M.empty
DInt 10
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinDiv) (AData $ DInt 10) ) (AData $ DInt 3)) M.empty
DInt 3
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinMod) (AData $ DInt 10) ) (AData $ DInt 3)) M.empty
DInt 1
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinLand) (AData $ DBool True) ) (AData $ DBool True)) M.empty
DBool True
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinLor) (AData $ DBool True) ) (AData $ DBool False)) M.empty
DBool True
>>> eval ( AFunApp (AData $DPrim builtinNeg) (AData $ DBool True) ) M.empty
DBool False
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinEq) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool True
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinNeq) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool False
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinLt) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool False
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinLe) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool True
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinGt) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool False
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtinGe) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool True
-}

