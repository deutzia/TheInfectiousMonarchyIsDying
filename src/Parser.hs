module Parser where

{-
writen based on: https://markkarpov.com/tutorial/megaparsec.html
-}

import Types

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Control.Monad.Combinators.Expr as E

type Parser = Parsec Void String

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
roperator s = try $ string s *> notFollowedBy (oneOf "=-+*/\\<>!%&|~") *> sc

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
pLIdentifierString =
    lexeme $ (:) <$> lowerChar <*> many alphaNumChar

-- lower case identifier
pLIdentifier :: Parser AST
pLIdentifier = do
    name <- pLIdentifierString
    return $ AVariable name

-- upper case identifier
pUIdentifier :: Parser AST
pUIdentifier = do
    name <- lexeme $ (:) <$> upperChar <*> many alphaNumChar
    return $ AVariable name

pLetVar :: Parser (String, AST)
pLetVar = do
    name <- pLIdentifierString
    roperator "="
    exp <- pExpr
    return (name, exp)

pLet :: Parser AST
pLet = do
    rstring "let"
    vars <- many pLetVar
    exp <- pExpr
    return $ ALet vars exp

pLambda :: Parser AST
pLambda = do
    rstring "fun"
    vars <- try (some pLIdentifierString)
    roperator "->"
    body <- pExpr
    return $ foldr ALambda body vars

pParens :: Parser a -> Parser a
pParens = between (symbol "(") (symbol ")")

pTerm :: Parser AST
pTerm = choice
  [ pParens pExpr
  , pLIdentifier
  , pUIdentifier
  , pInt
  , pLambda
  , pLet
  ]

pExpr :: Parser AST
pExpr = E.makeExprParser pTerm operatorTable

operatorTable :: [[E.Operator Parser AST]]
operatorTable =
  [ [ binary "" AFunApp
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

