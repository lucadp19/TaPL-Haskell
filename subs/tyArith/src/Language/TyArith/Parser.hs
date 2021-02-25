{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
The "Language.TyArith.Parser" module contains the several parsers used to implement the
typed arithmetic language. 
It exposes a single parsers, used to parse terms that need to be evaluated
(called @'parseTerm'@).
-}

module Language.TyArith.Parser 
    ( -- * Language parsers
      parseTerm
      -- ** Helpers
    , Parser
    , symbol
    ) where

import Language.TyArith.Syntax ( Term(..) )

import Data.Void ( Void )
import qualified Data.Text as T

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | The monadic parser.
type Parser = Parsec Void T.Text

-- | The whitespace lexer.
ws :: Parser ()
ws = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | The textual symbol parser.
symbol :: T.Text -> Parser T.Text
symbol = L.symbol ws

-- | The parser for parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

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

-- | Parses a literal, which could be `LitTrue`, `LitFalse`, `LitZero` or a term in parenthesis.
parseLiteral :: Parser Term
parseLiteral = choice
    [ parens parseTerm
    , LitTrue <$ symbol "True"
    , LitFalse <$ symbol "False"
    , LitZero  <$ symbol "0"
    ]
  
{- |
Parses a complex expression, such as
  - a term in parenthesis
  - succ <lit>
  - prec <lit>
  - isZero? <lit>
  - if-then-else-expression
  - a literal.
-}
parseComplex :: Parser Term
parseComplex = choice
    [ parens parseTerm
    , Succ   <$ symbol "succ"    <*> label err parseLiteral
    , Prec   <$ symbol "prec"    <*> label err parseLiteral
    , IsZero <$ symbol "isZero?" <*> label err parseLiteral
    , parseIf
    , parseLiteral
    ]
  where
    err :: String
    err = "a literal or a term in parenthesis"

-- | The parser for a TyArith term.
parseTerm :: Parser Term
parseTerm = parseComplex