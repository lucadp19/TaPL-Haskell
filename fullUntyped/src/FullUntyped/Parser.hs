{-# LANGUAGE OverloadedStrings #-}
module FullUntyped.Parser 
    ( -- * Language parsers
      parseTerm
    , parseLet
    ) where

import FullUntyped.Syntax ( Term(..) )
import FullUntyped.Environment
import FullUntyped.Monad ( Eval )

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

-- | The whitespace lexer.
ws :: Parser ()
ws = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | The parser for lexemes.
lexeme :: Parser T.Text -> Parser T.Text
lexeme = L.lexeme ws

-- | The textual symbol parser.
symbol :: T.Text -> Parser T.Text
symbol = L.symbol ws

-- | The parser for parentheses.
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Parses the name of an identifier.
identifier :: Parser T.Text
identifier = lexeme $ do
    ident <- some letterChar
    check $ T.pack ident
  where
    check :: T.Text -> Parser T.Text
    check name
        | name `elem` reserved = fail $ "keyword " <> T.unpack name <> " cannot be an identifier."
        | otherwise = pure name

-- | Parses a local variable and returns the correct De Bruijn index.
parseLocal :: Parser Term
parseLocal = do
    var <- identifier
    localVar <- asks $ getLocalIndex var
    case localVar of
        Just n  -> pure $ Var n
        Nothing -> mzero

-- | Parses a global variable and returns the corresponding expression.
parseGlobal :: Parser Term
parseGlobal = do
    var <- identifier
    globalTerm <- asks $ getGlobalTerm var
    case globalTerm of
        Just t  -> pure t
        Nothing -> mzero 

-- | Parses a lambda abstraction.
parseLambda :: Parser Term
parseLambda = do
    char '\\'
    boundVar <- identifier
    symbol "."
    -- @local (insertIntoLocals boundVar)@ adds @boundVar@ to the list of local variables
    body <- local (insertIntoLocals boundVar) parseTerm
    pure $ Lam boundVar body

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

-- | Parses an expression without function applications.
parseSingle :: Parser Term
parseSingle = parens parseTerm
          <|> parseLambda 
          <|> try parseLocal
          <|> try parseGlobal
          <|> parseTrue
          <|> parseFalse
          <|> parseZero
          <|> parseSucc
          <|> parsePrec
          <|> try parseIf
          <|> parseIsZero

-- | Parses a series of function applications.
parseApp :: Parser Term
parseApp = do
    apps <- some parseSingle
    pure $ foldl1' App apps

-- | The parser for a FullyUntyped term.
parseTerm :: Parser Term
parseTerm = parseApp

-- | The parser for a global let expression.
parseLet :: Parser (T.Text, Term)
parseLet = do
    globName <- identifier
    symbol "="
    globTerm <- parseTerm
    pure (globName, globTerm)