{-# Options -Wall -Wname-shadowing #-}
module Primitives where

import Eval
import Types

ii2i_type :: Type
ii2i_type = TFun TInt $ TFun TInt TInt
ii2b_type :: Type
ii2b_type = TFun TInt $ TFun TInt TBool
bb2b_type :: Type
bb2b_type = TFun TBool $ TFun TBool TBool

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
    in Primitive "builtinAdd" ii2i_type 2 helper

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
    in Primitive "builtinSub" ii2i_type 2 helper

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
    in Primitive "builtinMul" ii2i_type 2 helper

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
    in Primitive "builtinDiv" ii2i_type 2 helper

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
    in Primitive "builtinMod" ii2i_type 2 helper

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
    in Primitive "builtinLand" bb2b_type 2 helper

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
    in Primitive "builtinLor" bb2b_type 2 helper

builtinNeg :: Primitive
builtinNeg =
    let
        helper [d] = do
            u <- unlazy d
            case u of
                (DBool b) -> return $ DBool (not b)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinNeg" (TFun TBool TBool) 1 helper

builtinEq :: Primitive
builtinEq =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt i1, DInt i2) -> return $ DBool (i1 == i2)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinEq" ii2b_type 2 helper

builtinNeq :: Primitive
builtinNeq =
    let
        helper (d1 : [d2]) = do
            u1 <- unlazy d1
            u2 <- unlazy d2
            case (u1, u2) of
                (DInt i1, DInt i2) -> return $ DBool (i1 /= i2)
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinNeq" ii2b_type 2 helper

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
    in Primitive "builtinLt" ii2b_type 2 helper

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
    in Primitive "builtinLe" ii2b_type 2 helper

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
    in Primitive "builtinGt" ii2b_type 2 helper

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
    in Primitive "builtinGe" ii2b_type 2 helper

builtinIf :: Primitive
builtinIf =
    let
        helper [d1, d2, d3] = do
            ub <- unlazy d1
            case ub of
                (DBool b) -> return $ if b then d2 else d3
                _ -> fail "types mismatch"
        helper _ = fail "types mismatch"
    in Primitive "builtinIf" (TFun TBool $ TFun (TVariable "a") $ TFun (TVariable "a") (TVariable "a")) 3 helper

