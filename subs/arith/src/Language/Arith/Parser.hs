{-# LANGUAGE OverloadedStrings #-}

{- |
The "Language.Arith.Parser" module contains the parsers used to implement the
Arith language. 
-}
module Language.Arith.Parser 
    ( -- * Main parser
      parseTerm
    , parseFalse
    ) where

import Language.Arith.Syntax ( Term(..) )

import qualified Data.Text as T
import Data.Void ( Void )

import Text.Megaparsec
import Text.Megaparsec.Char ( space1 )
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

-- | The parser for the True literal.
parseTrue :: Parser Term
parseTrue = symbol "True" >> pure LitTrue

-- | The parser for the False literal.
parseFalse :: Parser Term
parseFalse = symbol "False" >> pure LitFalse

-- | The parser for the zero literal.
parseZero :: Parser Term
parseZero = symbol "0" >> pure LitZero

-- | The parser for the successor function.
parseSucc :: Parser Term
parseSucc = symbol "succ" >> (Succ <$> parseTerm)

-- | The parser for the predecessor function.
parsePrec :: Parser Term
parsePrec = symbol "prec" >> (Prec <$> parseTerm)

-- | The parser for an "isZero?" expression.
parseIsZero :: Parser Term
parseIsZero = symbol "isZero?" >> (IsZero <$> parseTerm)

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

-- | The parser for an Arith term.
parseTerm :: Parser Term
parseTerm = parseTrue
        <|> parseFalse
        <|> parseZero
        <|> try parseSucc
        <|> try parsePrec
        <|> try parseIf
        <|> try parseIsZero
        <|> parens parseTerm