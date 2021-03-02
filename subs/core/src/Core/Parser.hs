{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-} -- For @Token s ~ Char@

{- |
The "Core.Parser" module defines some lexer and parsers used by all the languages.
These parsers are not language-specific: they fulfill very general tasks.
-}

module Core.Parser
    ( -- * Lexers
      -- $lexers
      ws
    , lexeme
      -- * Basic parsers
      -- $parsers
    , symbol
    , parens
    , keysIdent 
    ) where

import Data.String ( IsString )
import Data.Text as T ( Text, pack, unpack )

import Text.Megaparsec
    ( between, some, MonadParsec, Stream(Token), label )
import Text.Megaparsec.Char ( char, letterChar, space1 )
import qualified Text.Megaparsec.Char.Lexer as L

{- $lexers
The 'ws' and 'lexeme' functions are the two basic lexemes and they are used to
strip the text to be parsed of whitespaces and comments.
-}

{- | 
The whitespace lexer: it is used in other lexers to remove whitespace and/or
line and block comments.
-}
ws :: MonadParsec e T.Text p 
   => p ()
ws = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

-- | The parser for lexemes.
lexeme :: MonadParsec e T.Text p
       => p s 
       -> p s
lexeme = L.lexeme ws

{- $parsers
This module defines three parsers:
    
    - 'symbol' parses a given text or fails,
    - 'parens' parses a text in parenthesis or fails,
    - 'keysIdent' takes a list of reserved keywords and tries to parse an identifier:
      if it parses a reserved keyword it fails with an error message.
-}

{- | 
The textual symbol parser: given a 'T.Text' symbol 
this parser parses that symbol or fails without consuming input.
-}
symbol :: MonadParsec e T.Text p
       => T.Text 
       -> p T.Text
symbol = L.symbol ws

{- | 
The parser for parentheses: parses something in parenthesis or
fails without consuming input.
 -}
parens :: ( MonadParsec e s p 
          , Token s ~ Char )
       => p a 
       -> p a
parens = between (char '(') (char ')')

{- |
Given a list of keywords this parser parses an identifier (which is a sequence of letters).
If the parsed identifier is not in the reserved keywords list the parser succeeds returning
the parsed id, otherwise it fails with an error message and consuming input. 
-}
keysIdent :: forall e p. 
             ( MonadParsec e T.Text p 
             , MonadFail p )
          => [T.Text]       -- ^ List of reserved keywords.
          -> p T.Text
keysIdent keys = lexeme $ do
    ident <- label "an identifier without numbers, symbols and underscores" $ some letterChar
    check $ T.pack ident
  where
    check :: T.Text -> p T.Text
    check name
        | name `elem` keys = fail $ "keyword " <> T.unpack name <> " cannot be an identifier."
        | otherwise = pure name