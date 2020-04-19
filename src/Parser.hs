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
import Types
import Primitives

type Parser = Parsec Void String

reserved :: [String]
reserved = ["def", "fun", "True", "False", "let", "in", "and", "if", "then", "else", "endif"]

-- whitespace cleaner
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
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

-- upper case identifier
pUIdentifier :: Parser AST
pUIdentifier = do
    name <- lexeme $ (:) <$> upperChar <*> many alphaNumChar
    if name `elem` reserved
        then fail $ "keyword " ++ name ++ " cannot be used as an identifier"
        else return $ AVariable name

pLetVar :: Parser (String, AST)
pLetVar = do
    name <- pLIdentifierString
    roperator "="
    expr <- pExp
    return (name, expr)

pLet :: Parser AST
pLet = do
    rstring "let"
    vars <- sepBy pLetVar (symbol ";")
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

pParens :: Parser a -> Parser a
pParens = between (symbol "(") (symbol ")")

pTerm :: Parser AST
pTerm = choice
  [ pParens pExp
  , pLambda
  , pLet
  , pIf
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

pExpr :: Parser TopLevelExp
pExpr = do
    expr <- pExp
    roperator ";;"
    return $ Expr expr

pProgram :: Parser Program
pProgram = between sc eof (many (pDef <|> pExpr) )

