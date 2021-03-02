{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
The "Language.Untyped.Parser" module contains the several parsers used to implement the
Untyped Lambda Calculus. It exposes two parsers: one for terms that need to be evaluated
(called @'parseTerm'@) and one for global let bindings (called @'parseLet'@).
-}

module Language.Untyped.Parser 
    ( -- * Language parsers
      parseTerm
    , parseLet
    ) where

import Core.Parser ( ws, symbol, parens, keysIdent )
import Core.Environment ( HasLocals(..), HasGlobals(..) )
import Core.Lenses ( HasTerm(..) )

import Language.Untyped.Syntax ( Term(..) )
import Language.Untyped.Environment ( createLocalBind )
import Language.Untyped.Monad ( Eval )

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

-- | Parses an identifier.
identifier :: Parser T.Text
identifier = keysIdent []

-- | Parses a local or global variable and returns the correct De Bruijn index or the correct global term.
parseVar :: Parser Term
parseVar = do
    var <- identifier
    asks (getLocalIndex var) >>= \case
        Just n  -> pure $ Var n
        Nothing -> asks (getGlobalBind var) >>= \case
            Just bind -> pure $ bind^.termL
            Nothing   -> fail $ "no local or global variable named " <> T.unpack var

-- | Parses a lambda abstraction.
parseLambda :: Parser Term
parseLambda = do
    char '\\'
    boundVar <- identifier
    symbol "."
    body <- let newBind = createLocalBind boundVar in
        local (insertIntoLocals newBind) parseTerm
    pure $ Lam boundVar body

-- | Parses a single variable or lambda abstraction.
parseSingle :: Parser Term
parseSingle = choice 
    [ parens parseTerm
    , parseLambda 
    , parseVar
    ]

-- | Parses a series of function applications.
parseApp :: Parser Term
parseApp = do
    apps <- some parseSingle
    pure $ foldl1' App apps

-- | The parser for an Untyped term.
parseTerm :: Parser Term
parseTerm = parseApp

-- | The parser for a global let expression.
parseLet :: Parser (T.Text, Term)
parseLet = do
    globName <- identifier
    symbol "="
    globTerm <- label "well-formed Untyped term" parseTerm
    pure (globName, globTerm)