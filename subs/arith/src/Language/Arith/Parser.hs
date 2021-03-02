{-# LANGUAGE OverloadedStrings #-}

{- |
The "Language.Arith.Parser" module contains the parsers used to implement the
Arith language. 
-}
module Language.Arith.Parser 
    ( -- * Main parser
      parseTerm
      -- ** Helper functions
    , Parser
    , symbol
    ) where

import Core.Parser ( ws, symbol, parens )

import Language.Arith.Syntax ( Term(..) )

import qualified Data.Text as T
import Data.Void ( Void )

import Text.Megaparsec
import Text.Megaparsec.Char ( space1 )
import qualified Text.Megaparsec.Char.Lexer as L

-- | The monadic parser.
type Parser = Parsec Void T.Text

-- | Parses a literal value: `LitTrue`, `LitFalse`, `LitZero` or a term in parentheses.
parseLiteral :: Parser Term
parseLiteral = choice
    [ parens parseTerm
    , LitZero  <$ symbol "0"
    , LitTrue  <$ symbol "True"
    , LitFalse <$ symbol "False"
    ]

-- | The parser for an if-then-else expression.
parseIf :: Parser Term
parseIf = do
    _ <- symbol "if"
    cond <- parseTerm
    _ <- symbol "then"
    trueExpr <- parseTerm
    _ <- symbol "else"
    falseExpr <- parseTerm
    pure $ IfThenElse cond trueExpr falseExpr
  
-- | The parser for a complete Arith expression.
parseTerm :: Parser Term
parseTerm = choice
    [ parens parseTerm
    , Succ   <$ symbol "succ"    <*> label expectLit parseLiteral
    , Prec   <$ symbol "prec"    <*> label expectLit parseLiteral
    , IsZero <$ symbol "isZero?" <*> label expectLit parseLiteral
    , parseIf
    , parseLiteral
    ]
  where
    expectLit :: String
    expectLit = "a literal value or a term in parentheses"