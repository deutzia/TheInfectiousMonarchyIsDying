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
>>> eval ( ALet [("x", AData $ DInt 2), ("y", AData $ DInt 3)] ( AFunApp ( AFunApp (AData $ DPrim builtin_add) (AVariable "x") ) (AVariable "y"))) M.empty
DInt 5
>>> eval (AFunApp (AData (DFun "x" ( AFunApp ( AFunApp (AData $ DPrim builtin_mul) (AVariable "x") ) (AVariable "x")) M.empty)) (AData $ DInt 5)) M.empty
DInt 25
-}

builtin_add :: Primitive
builtin_add =
    let
        helper ((DInt n):[DInt m]) = DInt (n + m)
        helper _ = undefined
    in Primitive "builtin_add" 2 helper

builtin_sub :: Primitive
builtin_sub =
    let
        helper ((DInt n):[DInt m]) = DInt (n - m)
        helper _ = undefined
    in Primitive "builtin_sub" 2 helper

builtin_mul :: Primitive
builtin_mul =
    let
        helper ((DInt n):[DInt m]) = DInt (n * m)
        helper _ = undefined
    in Primitive "builtin_mul" 2 helper

builtin_div :: Primitive
builtin_div =
    let
        helper ((DInt n):[DInt m]) = DInt (n `div` m)
        helper _ = undefined
    in Primitive "builtin_div" 2 helper

builtin_mod :: Primitive
builtin_mod =
    let
        helper ((DInt n):[DInt m]) = DInt (n `mod` m)
        helper _ = undefined
    in Primitive "builtin_mod" 2 helper

builtin_land :: Primitive
builtin_land =
    let
        helper ((DBool b1):[DBool b2]) = DBool (b1 && b2)
        helper _ = undefined
    in Primitive "builtin_land" 2 helper

builtin_lor :: Primitive
builtin_lor =
    let
        helper ((DBool b1):[DBool b2]) = DBool (b1 || b2)
        helper _ = undefined
    in Primitive "builtin_lor" 2 helper

builtin_neg :: Primitive
builtin_neg =
    let
        helper [DBool b] = DBool (not b)
        helper _ = undefined
    in Primitive "builtin_neg" 1 helper

builtin_eq :: Primitive
builtin_eq =
    let
        helper ((DInt n):[DInt m]) = DBool (n == m)
        helper _ = undefined
    in Primitive "builtin_eq" 2 helper

builtin_neq :: Primitive
builtin_neq =
    let
        helper ((DInt n):[DInt m]) = DBool (n /= m)
        helper _ = undefined
    in Primitive "builtin_neq" 2 helper

builtin_lt :: Primitive
builtin_lt =
    let
        helper ((DInt n):[DInt m]) = DBool (n < m)
        helper _ = undefined
    in Primitive "builtin_lt" 2 helper

builtin_le :: Primitive
builtin_le =
    let
        helper ((DInt n):[DInt m]) = DBool (n <= m)
        helper _ = undefined
    in Primitive "builtin_le" 2 helper

builtin_gt :: Primitive
builtin_gt =
    let
        helper ((DInt n):[DInt m]) = DBool (n > m)
        helper _ = undefined
    in Primitive "builtin_gt" 2 helper

builtin_ge :: Primitive
builtin_ge =
    let
        helper ((DInt n):[DInt m]) = DBool (n >= m)
        helper _ = undefined
    in Primitive "builtin_ge" 2 helper

{- | Primitives
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_add) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DInt 4
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_sub) (AData $ DInt 5) ) (AData $ DInt 2)) M.empty
DInt 3
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_mul) (AData $ DInt 5) ) (AData $ DInt 2)) M.empty
DInt 10
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_div) (AData $ DInt 10) ) (AData $ DInt 3)) M.empty
DInt 3
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_mod) (AData $ DInt 10) ) (AData $ DInt 3)) M.empty
DInt 1
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_land) (AData $ DBool True) ) (AData $ DBool True)) M.empty
DBool True
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_lor) (AData $ DBool True) ) (AData $ DBool False)) M.empty
DBool True
>>> eval ( AFunApp (AData $DPrim builtin_neg) (AData $ DBool True) ) M.empty
DBool False
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_eq) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool True
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_neq) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool False
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_lt) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool False
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_le) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool True
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_gt) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool False
>>> eval ( AFunApp ( AFunApp (AData $DPrim builtin_ge) (AData $ DInt 2) ) (AData $ DInt 2)) M.empty
DBool True
-}

