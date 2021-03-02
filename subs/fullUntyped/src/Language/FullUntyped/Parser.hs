{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
The "Language.FullUntyped.Parser" module contains the several parsers used to implement the
Untyped Lambda Calculus. It exposes two parsers: one for terms that need to be evaluated
(called @'parseTerm'@) and one for global let bindings (called @'parseLet'@).
-}

module Language.FullUntyped.Parser 
    ( -- * Language parsers
      parseTerm
    , parseLet
    ) where

import Core.Parser
    ( ws, lexeme, symbol, parens, keysIdent )
import Core.Lenses ( HasTerm(termL) ) 

import Language.FullUntyped.Syntax ( Term(..) )
import Language.FullUntyped.Environment
import Language.FullUntyped.Monad ( Eval )

import Lens.Micro ( (^.) )

import Data.Void ( Void )
import Data.List ( foldl1' )
import qualified Data.Text as T
import Control.Monad.Reader

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | The monadic parser with a lexical scope given by the @Eval@ monad.
type Parser = ParsecT Void T.Text Eval

-- | List of reserved words.
reserved :: [T.Text]
reserved = ["if", "then", "else", "succ", "prec", "isZero?", "True", "False"]

-- | Parses the name of an identifier.
identifier :: Parser T.Text
identifier = keysIdent reserved

-- | Parses a local or global variable and returns the correct De Bruijn index or the correct global term.
parseVar :: Parser Term
parseVar = do
    var <- identifier
    asks (getLocalIndex var) >>= \case
        Just n  -> pure $ Var n
        Nothing -> asks (getGlobalBind var) >>= \case
            Just bind -> pure $ bind^.termL
            Nothing   -> fail  $ "no local or global variable named " <> T.unpack var

-- | Parses a lambda abstraction.
parseLambda :: Parser Term
parseLambda = do
    char '\\'
    boundVar <- label "identifier" identifier
    symbol "."
    body <- local (insertIntoLocals $ createLocalBind boundVar) parseTerm
    pure $ Lam boundVar body

-- | Parses a literal, which can be `LitTrue`, `LitFalse`, `LitZero`, a variable or a term in parenthesis.
parseLiteral :: Parser Term
parseLiteral = choice
    [ parens parseTerm
    , LitTrue  <$ symbol "True"
    , LitFalse <$ symbol "False"
    , LitZero  <$ symbol "0"
    , parseVar
    ]

-- | Parses a non-top-level function application.
parseApp :: Parser Term
parseApp = do
    apps <- some parseLiteral
    pure $ foldl1' App apps

{- | 
Parses a complex expression, such as
    - succ <f-app>
    - prec <f-app>
    - isZero? <f-app>
    - if-then-else
    - lambda abstraction
    - function application
    - term in parenthesis.
-}
parseComplex :: Parser Term
parseComplex = choice
    [ parens parseTerm
    , Succ   <$ symbol "succ"    <*> label err parseApp
    , Prec   <$ symbol "prec"    <*> label err parseApp
    , IsZero <$ symbol "isZero?" <*> label err parseApp
    , parseIf
    , parseLambda
    , parseApp
    ]
  where
    err :: String
    err = "function application, literal or term in parenthesis"

-- | Parses a FullUntyped term.
parseTerm :: Parser Term
parseTerm = do
    apps <- some parseComplex
    pure $ foldl1' App apps

-- | The parser for an if-then-else expression.
parseIf :: Parser Term
parseIf = do
    symbol "if"
    cond <- parseTerm
    symbol "then"
    trueExpr <- parseTerm
    symbol "else"
    falseExpr <- parseTerm
    pure $ IfThenElse cond trueExpr falseExpr

-- | The parser for a global let expression.
parseLet :: Parser (T.Text, Term)
parseLet = do
    globName <- identifier
    symbol "="
    globTerm <- parseTerm
    pure (globName, globTerm)