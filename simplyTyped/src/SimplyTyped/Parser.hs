{-# LANGUAGE OverloadedStrings #-}

{-|
The "SimplyTyped.Parser" module contains the several parsers used to implement the
Simply Typed Lambda Calculus. It exposes two parsers: one for terms that need to be evaluated
(called @'parseTerm'@) and one for global let bindings (called @'parseLet'@).
-}

module SimplyTyped.Parser 
    ( -- * Language parsers
      parseTerm
    , parseLet
    ) where

import SimplyTyped.Types ( Typ(..) )
import SimplyTyped.Syntax ( Term(..) )
import SimplyTyped.Env
import SimplyTyped.Monad ( Eval )

import Data.Void ( Void )
import Data.List ( foldl1' )
import qualified Data.Text as T
import Control.Monad.Reader

import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

-- | The monadic parser with a lexical scope given by the @Eval@ monad.
type Parser = ParsecT Void T.Text Eval

-- | List of reserved words.
reserved :: [T.Text]
reserved = ["if", "then", "else", "succ", "prec", "isZero?", "True", "False", "Nat", "Bool"]

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
    symbol ":"
    boundTyp <- parseType
    symbol "."
    -- @local (insertIntoLocals (boundVar, boundTyp))@ adds 
    --      a variable with the given name and type to the list of local variables
    body <- local (insertIntoLocals (boundVar, boundTyp)) parseTerm
    pure $ Lam boundVar boundTyp body

-- | Parses a single type annotation.
parseSingleType :: Parser Typ
parseSingleType = choice
    [ parens parseArrow
    , Bool <$ symbol "Bool"
    , Nat <$ symbol "Nat" ]

-- | Parses an @Arr@ow type annotation.
parseArrow :: Parser Typ
parseArrow = makeExprParser parseSingleType operatorTable
  where
    operatorTable :: [[Operator Parser Typ]]
    operatorTable = [ [ binary "->" Arr ] ]
    binary :: T.Text -> (Typ -> Typ -> Typ) -> Operator Parser Typ
    binary name f = InfixL (f <$ symbol name)

-- | The parser for type annotations.
parseType :: Parser Typ
parseType = parseArrow

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

-- | The parser for a SimplyTyped term.
parseTerm :: Parser Term
parseTerm = parseApp

-- | The parser for a global let expression.
parseLet :: Parser (T.Text, Term)
parseLet = do
    globName <- identifier
    symbol "="
    globTerm <- parseTerm
    pure (globName, globTerm)