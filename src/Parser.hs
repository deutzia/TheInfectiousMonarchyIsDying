{-# Options -Wall -Wname-shadowing #-}
module Parser where

{-
writen based on: https://markkarpov.com/tutorial/megaparsec.html
-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as E
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Control.Monad
import Data.Maybe
import Data.List
import Data.Ord

import Types
import Eval
import Primitives

type Parser = Parsec Void String

reserved :: [String]
reserved = ["def", "fun", "True", "False", "let", "in", "and", "if", "then", "else", "endif", "Bool", "Int", "typedef", "match"]

-- whitespace cleaner
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "/-")
  (L.skipBlockComment "//" "\\\\")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

rstring :: String -> Parser ()
rstring s = try $ string s *> notFollowedBy (alphaNumChar <|> char '_') *> sc

roperator :: String -> Parser ()
roperator s = try $ string s *> notFollowedBy (oneOf "=-+*/\\<>!%&|~;") *> sc

-- Expression parsers
pBool :: Parser AST
pBool = do
    b <- (rstring "True" *> pure True) <|> (rstring "False" *> pure False)
    return $ AData $ DBool b
{- | pBool
>>> parseTest pBool "True"
AData DBool True
-}

pInt :: Parser AST
pInt = do
    n <- lexeme L.decimal
    return $ AData $ DInt n
{- | pInt
>>> parseTest pInt "3"
AData DInt 3
-}

pLIdentifierString :: Parser String
pLIdentifierString = do
    name <- lexeme $ (:) <$> lowerChar <*> many alphaNumChar
    if name `elem` reserved
        then fail $ "keyword " ++ name ++ " cannot be used as an identifier"
        else return name

-- lower case identifier
pLIdentifier :: Parser AST
pLIdentifier = AVariable <$> pLIdentifierString

pUIdentifierString :: Parser String
pUIdentifierString = do
    name <- lexeme $ (:) <$> upperChar <*> many alphaNumChar
    if name `elem` reserved
        then fail $ "keyword " ++ name ++ " cannot be used as an identifier"
        else return name

-- upper case identifier
pUIdentifier :: Parser AST
pUIdentifier = AVariable <$> pUIdentifierString

pLetVar :: Parser (String, AST)
pLetVar = do
    name <- pLIdentifierString
    roperator "="
    expr <- pExp
    return (name, expr)

pLet :: Parser AST
pLet = do
    rstring "let"
    -- at least one thing should be defined inside of an if
    vars <- sepBy1 pLetVar (symbol ";")
    rstring "in"
    ALet vars <$> pExp

pLambda :: Parser AST
pLambda = do
    rstring "fun"
    vars <- try (some pLIdentifierString)
    roperator "->"
    body <- pExp
    return $ foldr ALambda body vars

pIf :: Parser AST
pIf = do
    rstring "if"
    cond <- pExp
    rstring "then"
    e1 <- pExp
    rstring "else"
    e2 <- pExp
    rstring "endif"
    return $ AFunApp (AFunApp (AFunApp (AData $ DPrim builtinIf) cond) e1) e2

pMexp :: Parser (String, [String], AST)
pMexp = do
    roperator "|"
    name <- pUIdentifierString
    vars <- many pLIdentifierString
    roperator "->"
    e <- pExp
    return (name, vars, foldr ALambda e vars)

pMatch :: Parser AST
pMatch = do
    rstring "match"
    e <- pExp
    clauses <- some (pMexp)
    rstring "endmatch"
    return $ AMatch e clauses

pParens :: Parser a -> Parser a
pParens = between (symbol "(") (symbol ")")

pTerm :: Parser AST
pTerm = choice
  [ pParens pExp
  , pLambda
  , pLet
  , pIf
  , pMatch
  , pBool
  , pLIdentifier
  , pUIdentifier
  , pInt
  ]

pExp :: Parser AST
pExp = E.makeExprParser pTerm operatorTable

operatorTable :: [[E.Operator Parser AST]]
operatorTable =
  [ [ binary "." AFunApp
    ]
  , [ binary "*" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinMul) l) r)
    , binary "/" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinDiv) l) r)
    , binary "%" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinMod) l) r)
    ]
  , [ binary "+" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinAdd) l) r)
    , binary "-" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinSub) l) r)
    ]
  , [ binary ">=" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinGe) l) r)
    , binary ">" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinGt) l) r)
    , binary "<=" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinLe) l) r)
    , binary "<" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinLt) l) r)
    , binary "==" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinEq) l) r)
    , binary "!=" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinNeq) l) r)
    ]
  , [ binary "||" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinLor) l) r)
    , binary "&&" (\l r -> AFunApp (AFunApp (AData $ DPrim builtinLand) l) r)
    ]
  , [ prefix "~" (AFunApp (AData $ DPrim builtinNeg))
    ]
  ]

binary :: String -> (AST -> AST -> AST) -> E.Operator Parser AST
binary  name f = E.InfixL  (f <$ symbol name)

prefix, postfix :: String -> (AST -> AST) -> E.Operator Parser AST
prefix  name f = E.Prefix  (f <$ symbol name)
postfix name f = E.Postfix (f <$ symbol name)

{- | Parsing expressions
>>> eval (fromRight ( AData $ DInt 0 ) (runParser pExp "test" "1 + 2 + 3 + 4")) M.empty
DInt 10
>>> eval (fromRight ( AData $ DInt 0 ) (runParser pExp "test" "1 + 2 + 3 + 5")) M.empty
DInt 11
>>> eval (fromRight ( AData $ DInt 0 ) (runParser pExp "test" "1 + 2 * 3 + 4")) M.empty
DInt 11
>>> eval (fromRight ( AData $ DInt 0 ) (runParser pExp "test" "1 + 2 * (3 + 4)")) M.empty
DInt 15
>>> eval (fromRight ( AData $ DInt 0 ) (runParser pExp "test" "let y = 3 in y * y")) M.empty
DInt 9
>>> eval (fromRight ( AData $ DInt 0 ) (runParser pExp "test" "let iin = 3 in iin * iin")) M.empty
DInt 9
>>> eval (fromRight ( AData $ DInt 0 ) (runParser pExp "test" "let f = fun x -> x * x in f . 3")) M.empty
DInt 9
>>> eval (fromRight ( AData $ DInt 0 ) (runParser pExp "test" "let a = 1; b = 2 in a + b")) M.empty
DInt 3
>>> parseTest pExp "leti"
AVariable "leti"
>>>> eval (fromRight ( AData $ DInt 0 ) (runParser pExp "test" "let f = fun x y -> x * y ; a = 3; b = 2 in f . a . b")) M.empty
DInt 6
>>> eval (fromRight (AData $ DInt 0) (runParser pExp "test" "if 2 < 3 then 4 else 5 endif")) M.empty
DInt 4
>>> eval (fromRight (AData $ DInt 0) (runParser pExp "test" "let factorial = fun x -> if x < 2 then 1 else x * factorial . (x-1) endif in factorial . 5")) M.empty
DInt 120
-}

pDef :: Parser TopLevelExp
pDef = do
    rstring "def"
    name <- pLIdentifierString
    vars <- try (many pLIdentifierString)
    roperator "="
    body <- pExp
    roperator ";;"
    return $ Def name (foldr ALambda body vars)

{- | Pasring top level definitions
>>> parseTest pDef "def x = 3 ;; "
Def "x" (AData DInt 3)
>>> parseTest pDef "def f x y = x * y ;; "
Def "f" (ALambda "x" (ALambda "y" (AFunApp (AFunApp (AData DPrim builtinMul) (AVariable "x")) (AVariable "y"))))
-}

pTBool :: Parser Type
pTBool = do
    rstring "Bool"
    return TBool

pTInt :: Parser Type
pTInt = do
    rstring "Int"
    return TInt

pTVar :: Parser Type
pTVar = do
    name <- pLIdentifierString
    return $ TVariable $ "user_" ++ name

pTFun :: Parser Type
pTFun = try $ do
    t1 <- pTypeSingle
    roperator "->"
    t2 <- pType
    return $ TFun t1 t2

pTypeSingle :: Parser Type
pTypeSingle =
    pTBool
    <|> pTInt
    <|> pTAlgebraic
    <|> pTVar
    <|> pParens pType

pType :: Parser Type
pType =
    pTypeSingle
    <|> pTFun

pTAlgebraic :: Parser Type
pTAlgebraic = do
    name <- pUIdentifierString
    types <- many pType
    return $ TAlgebraic name types

pTypedefConstructor :: Parser (String, [Type])
pTypedefConstructor = do
    name <- pUIdentifierString
    types <- many pType
    return (name, types)

pTypedef :: Parser TopLevelExp
pTypedef = do
    rstring "typedef"
    name <- pUIdentifierString
    -- many because type doesn't have to be parametrised
    typeVars <- try (many pTVar)
    roperator "="
    -- there has to be at least one constructor
    constructors <- sepBy1 pTypedefConstructor (symbol "|")
    roperator ";;"
    let retType = TAlgebraic name typeVars
    let constructors' = sortBy (comparing fst) constructors
    -- constructors are just functions, but they know their own type, so
    -- might as well treat them as primitives
    let {primitives =
        map
            (
                \(primName, types) ->
                    let
                        primType = foldr TFun retType types
                        primFun = \d -> do
                            return $ DAlgebraic primName d
                    in Primitive primName primType (length types) primFun
            )
            constructors'}

    let cNames = reverse $ map fst constructors'
    let matchRetType = TVariable "_match_returned_type"
    let matchClauseTypes = reverse $ map (\(_, args) -> foldr TFun matchRetType args) constructors'
    let mType = foldr TFun (TFun retType matchRetType) matchClauseTypes
    let mFun = (\args -> do
            let funs = init args
            actualArg <- unlazy $ last args
            case actualArg of
                DAlgebraic cName vals -> do
                    return $ foldr
                        (
                            \(f, n) acc ->
                                if n == cName
                                    then foldl DLazyApp f vals
                                    else acc
                        )
                        DUndefined
                        (zip funs $ cNames)
                _ -> undefined
            )

    let matchPrim = Primitive ("__match_" ++ name) mType (1 + length constructors) mFun
    return $ Algebraic name (length typeVars) primitives matchPrim

pExpr :: Parser TopLevelExp
pExpr = do
    expr <- pExp
    roperator ";;"
    return $ Expr expr

pProgram :: Parser Program
pProgram = do
    program <- between sc eof (many (pTypedef <|> pDef <|> pExpr))
    program' <- postprocessProgram program
    return program'

fixMatch :: ConstrMap -> TypeMap -> AST -> Parser AST
fixMatch cm tm (AFunApp t1 t2) = do
    t1' <- fixMatch cm tm t1
    t2' <- fixMatch cm tm t2
    return $ AFunApp t1' t2'
fixMatch cm tm (ALambda name t) = do
    t' <- fixMatch cm tm t
    return $ ALambda name t'
fixMatch cm tm (ALet l t) = do
    t' <- fixMatch cm tm t
    l' <- mapM
        (\(n, tree) -> do
            tree' <- fixMatch cm tm tree
            return (n, tree')
        )
        l
    return $ ALet l' t'
fixMatch cm tm (AMatch t l) = do
    t' <- fixMatch cm tm t
    l' <- mapM
        (\(name, vars, tree) -> do
            tree' <- fixMatch cm tm tree
            return (name, vars, tree')
        )
        l
    types <- mapM
        (\(name, vars, _) -> do
            case M.lookup name cm of
                Nothing -> fail $ "Unbound constructor: " ++ name
                Just (cType, n) ->
                    if n /= length vars
                        then fail $ "Wrong number of arguments passed to constructor " ++ name
                        else return cType
        )
        l'
    let typesS = foldl (flip $ S.insert) S.empty types
    when (S.size typesS /= 1) (fail $ "constructors in match are of different types")
    let typename = head types
    when ((fromJust $ M.lookup typename tm) /= length l') $ fail $ "incomplete match"
    let lSorted = sortBy (comparing (\(n, _, _) -> n)) l'
    let funs = map (\(_, _, tree) -> tree) lSorted
    return $ foldr (flip $ AFunApp) (AVariable $ "__match_" ++ typename) (t' : funs)
fixMatch _ _ t = return t


-- map from constructors to their types and number of arguments
type ConstrMap = M.Map String (String, Int)

-- map from type names to numers of clauses
type TypeMap = M.Map String Int

-- Check whether matches are full
-- Change matches to function calls
postprocessProgram :: Program -> Parser Program
postprocessProgram prog =
    let
        constrHelper :: ConstrMap -> TopLevelExp -> ConstrMap
        constrHelper m (Def _ _) = m
        constrHelper m (Expr _) = m
        constrHelper m (Algebraic name _ ctors _) =
            foldl (\m' (Primitive pname _ n _) -> M.insert pname (name, n) m') m ctors
        constrMap = foldl constrHelper M.empty prog
        typeHelper :: TypeMap -> TopLevelExp -> TypeMap
        typeHelper m (Def _ _) = m
        typeHelper m (Expr _) = m
        typeHelper m (Algebraic name _ ctors _) = M.insert name (length ctors) m
        typeMap = foldl typeHelper M.empty prog
        matchHelper :: TopLevelExp -> Parser TopLevelExp
        matchHelper (Def name tree) = do
            tree' <- fixMatch constrMap typeMap tree
            return $ Def name tree'
        matchHelper tle@(Algebraic _ _ _ _) = return tle
        matchHelper (Expr t) = do
            t' <- fixMatch constrMap typeMap t
            return $ Expr t'
    in mapM matchHelper prog
