module Primitives where

import Types

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

