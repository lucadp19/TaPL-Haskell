{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
The "Language.SimplyTyped.Parser" module contains the several parsers used to implement the
Simply Typed Lambda Calculus. It exposes two parsers: one for terms that need to be evaluated
(called @'parseTerm'@) and one for global let bindings (called @'parseLet'@).
-}

module Language.SimplyTyped.Parser 
    ( -- * Language parsers
      parseTerm
    , parseLet
    ) where

import Language.SimplyTyped.Types ( Typ(..) )
import Language.SimplyTyped.Syntax ( Term(..) )
import Language.SimplyTyped.Env
import Language.SimplyTyped.Monad ( Eval )

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

-- | Parses a single type annotation.
parseSingleType :: Parser Typ
parseSingleType = choice
    [ parens parseArrowTy
    , Bool <$ symbol "Bool"
    , Nat <$ symbol "Nat" 
    ]

-- | Parses an @Arr@ow type annotation.
parseArrowTy :: Parser Typ
parseArrowTy = makeExprParser parseSingleType 
    [ [ InfixL $ Arr <$ symbol "->" ] ]

-- | The parser for type annotations.
parseType :: Parser Typ
parseType = parseArrowTy

-- | Parses a local or global variable and returns the correct De Bruijn index or the corresponding global term.
parseVar :: Parser Term
parseVar = do
    var <- identifier
    asks (getLocalIndex var) >>= \case
        Just n  -> pure $ Var n
        Nothing -> asks (getGlobalTerm var) >>= \case
            Just term -> pure term
            Nothing   -> fail $ "no local or global variable named " <> T.unpack var

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

-- | Parses a literal, which can be `LitZero`, `LitTrue`, `LitFalse`, a variable or a term in parenthesis.
parseLiteral :: Parser Term
parseLiteral = choice
    [ parens parseTerm
    , LitTrue  <$ symbol "True"
    , LitFalse <$ symbol "False"
    , LitZero  <$ symbol "0" 
    , parseVar 
    ]

-- | Parses a function application.
parseApp :: Parser Term
parseApp = makeExprParser parseLiteral 
    [ [ InfixL $ App <$ symbol "" ] ]

{- |
Parses a complex expression, which can be
    - a term in parenthesis
    - Succ <app>
    - Prec <app>
    - IsZero <app>
    - if expression
    - lambda expression
    - function application.
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
    err = "a function application, a literal or a term in parenthesis"

-- | Parses an "if-then-else" expression.
parseIf :: Parser Term
parseIf = do
    symbol "if"
    cond <- parseTerm
    symbol "then"
    trueT <- parseTerm
    symbol "else"
    falseT <- parseTerm
    pure $ IfThenElse cond trueT falseT


-- | The parser for a SimplyTyped term.
parseTerm :: Parser Term
parseTerm = makeExprParser parseComplex
    [ [InfixL (App <$ symbol "")] ]

-- | The parser for a global let expression.
parseLet :: Parser (T.Text, Term)
parseLet = do
    globName <- identifier
    symbol "="
    globTerm <- parseTerm
    pure (globName, globTerm)