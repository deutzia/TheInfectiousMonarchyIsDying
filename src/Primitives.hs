{-# Options -Wall -Wname-shadowing #-}
module Primitives where

import Eval
import Types

builtinAdd :: Primitive
builtinAdd =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt n, DInt m) -> return $ DInt (n + m)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinAdd" 2 helper

builtinSub :: Primitive
builtinSub =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt n, DInt m) -> return $ DInt (n - m)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinSub" 2 helper

builtinMul :: Primitive
builtinMul =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt n, DInt m) -> return $ DInt (n * m)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinMul" 2 helper

builtinDiv :: Primitive
builtinDiv =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt n, DInt m) -> if m == 0
                    then fail "division by zero"
                    else return $ DInt (n `div` m)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinDiv" 2 helper

builtinMod :: Primitive
builtinMod =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt n, DInt m) -> if m == 0
                    then fail "modulo by zero"
                    else return $ DInt (n `mod` m)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinMod" 2 helper

builtinLand :: Primitive
builtinLand =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            case u1 of
                DBool b1 -> if b1
                    then do
                        u2 <- unlazy d2
                        case u2 of
                            DBool b2 -> return $ DBool b2
                            _ -> fail "types mismatch"
                    else return $ DBool False
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinLand" 2 helper

builtinLor :: Primitive
builtinLor =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            case u1 of
                DBool b1 -> if not b1
                    then do
                        u2 <- unlazy d2
                        case u2 of
                            DBool b2 -> return $ DBool b2
                            _ -> fail "types mismatch"
                    else return $ DBool True
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinLor" 2 helper

builtinNeg :: Primitive
builtinNeg =
    let
        helper [d] = do
            u <- unlazy d
            case u of
                (DBool b) -> return $ DBool (not b)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinNeg" 1 helper

builtinEq :: Primitive
builtinEq =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt i1, DInt i2) -> return $ DBool (i1 == i2)
                (DBool i1, DBool i2) -> return $ DBool (i1 == i2)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinEq" 2 helper

builtinNeq :: Primitive
builtinNeq =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt i1, DInt i2) -> return $ DBool (i1 /= i2)
                (DBool i1, DBool i2) -> return $ DBool (i1 /= i2)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinNeq" 2 helper

builtinLt :: Primitive
builtinLt =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt n, DInt m) -> return $ DBool (n < m)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinLt" 2 helper

builtinLe :: Primitive
builtinLe =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt n, DInt m) -> return $ DBool (n <= m)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinLe" 2 helper

builtinGt :: Primitive
builtinGt =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt n, DInt m) -> return $ DBool (n > m)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinGt" 2 helper

builtinGe :: Primitive
builtinGe =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt n, DInt m) -> return $ DBool (n >= m)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinGe" 2 helper

builtinIf :: Primitive
builtinIf =
    let
        helper [d1, d2, d3] = do
            ub <- unlazy d1
            case ub of
                (DBool b) -> return $ if b then d2 else d3
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinIf" 3 helper

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

