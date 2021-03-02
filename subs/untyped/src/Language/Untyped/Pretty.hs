{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

{- |
The "Language.Untyped.Pretty" module contains some helper functions for pretty-printing
terms and REPL results.
-}

module Language.Untyped.Pretty 
    ( -- * Main prettyfing function
      prettyTerm
    ) where

import Core.Environment ( HasLocals(..) )
import Core.Lenses ( HasName(..) )
import Core.Pretty ( text )

import Language.Untyped.Syntax ( Term(..) )
import Language.Untyped.Environment ( createLocalBind, LocalBind )

import Lens.Micro ( (^.) )

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
    ( Doc, (<+>), parens, Pretty(pretty) )
import Control.Monad.Reader ( MonadReader(local), asks )

{- |
@'prettyTerm'@ takes a @'Term'@ and returns a prettyfied version contained in a 
monadic environment containing local variables.

The canonical choice for the monad is @'Language.Untyped.Monad.Eval'@.
-}
prettyTerm :: ( MonadReader env m
              , HasLocals env LocalBind )
           => Term 
           -> m (Doc ann)
prettyTerm = \case
    Var k -> asks (getLocalBind k) >>= \case
        Just bind -> pure $ pretty $ bind^.nameL
        Nothing   -> error "This cannot happen"
    Lam name body -> (lamText name <+>)
        <$> local (insertIntoLocals $ createLocalBind name) (prettyTerm body)
    App t1 t2 -> (<+>) <$> appParens t1 <*> appParens t2
  where
    lamText :: T.Text -> Doc ann
    lamText name = text "λ" <> pretty name <> text "."

-- | Surrounds the prettyfied version of a term with parentheses.
appParens :: ( MonadReader env m
             , HasLocals env LocalBind )
          => Term 
          -> m (Doc ann)
appParens t = case t of
    Var _ -> prettyTerm t
    _     -> parens <$> prettyTerm t