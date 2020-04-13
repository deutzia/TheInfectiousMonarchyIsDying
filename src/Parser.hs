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

-- Expression parsers
pBool :: Parser AST
pBool = do
    b <- (rstring "True" *> pure True) <|> (rstring "False" *> pure False)
    return $ AData $ DBool b
{- | pBool
>>> parseTest pBool "True"
AData DBool True
>>> parseTest pBool "Trueeee"
unxepected 'e'
-}

pInt :: Parser AST
pInt = do
    n <- lexeme L.decimal
    return $ AData $ DInt n
{- | pInt
>>> parseTest pInt "3"
AData DInt 3
-}

-- lower case identifier
pLIdentifier :: Parser AST
pLIdentifier = do
    name <- lexeme $ (:) <$> lowerChar <*> many alphaNumChar
    return $ AVariable name

-- upper case identifier
pUIdentifier :: Parser AST
pUIdentifier = do
    name <- lexeme $ (:) <$> upperChar <*> many alphaNumChar
    return $ AVariable name

pParens :: Parser a -> Parser a
pParens = between (symbol "(") (symbol ")")

pTerm :: Parser AST
pTerm = choice
  [ pParens pExpr
  , pLIdentifier
  , pUIdentifier
  , pInt
  ]

pExpr :: Parser AST
pExpr = E.makeExprParser pTerm operatorTable

operatorTable :: [[E.Operator Parser AST]]
operatorTable =
  [ [ binary "" (\l r -> AFunApp l r)
    ]
  , [ binary "*" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_mul) l) r)
    , binary "/" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_div) l) r)
    , binary "%" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_mod) l) r)
    ]
  , [ binary "+" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_add) l) r)
    , binary "-" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_sub) l) r)
    ]
  , [ binary ">=" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_ge) l) r)
    , binary ">" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_gt) l) r)
    , binary "<=" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_le) l) r)
    , binary "<" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_lt) l) r)
    , binary "==" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_eq) l) r)
    , binary "!=" (\l r -> AFunApp (AFunApp (AData $ DPrim builtin_neq) l) r)
    ]
  ]

binary :: String -> (AST -> AST -> AST) -> E.Operator Parser AST
binary  name f = E.InfixL  (f <$ symbol name)

prefix, postfix :: String -> (AST -> AST) -> E.Operator Parser AST
prefix  name f = E.Prefix  (f <$ symbol name)
postfix name f = E.Postfix (f <$ symbol name)

