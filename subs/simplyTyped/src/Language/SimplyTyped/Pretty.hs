{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
The "Language.SimplyTyped.Pretty" module contains some helper functions for pretty-printing
terms and REPL results.
-}

module Language.SimplyTyped.Pretty 
    ( -- * Main prettyfing function
      prettyTerm
      -- * Helpers
    , text
      -- ** Arrows
    , evalArrow
    , stepArrow
    , lastStepArrow
    ) where

import Language.SimplyTyped.Syntax ( Term(..) )
import Language.SimplyTyped.Types ( Typ )
import Language.SimplyTyped.Env
    ( HasLocals(insertIntoLocals, getLocalVar), nameL )

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

import Control.Monad.Reader ( MonadReader(local), asks )

import Lens.Micro.Extras ( view )

-- | Helper function for prettifying Text.
text :: T.Text -> Doc ann
text = pretty

{- | 
The arrow representing multistep evaluation:
it corresponds to Unicode character 21D2 (⇒).
-}
evalArrow :: Doc ann
evalArrow = text (" ⇒ " :: T.Text)

{- |
The arrow representing a single evaluation step:
it corresponds to Unicode character 2192 (→).
-}
stepArrow :: Doc ann
stepArrow = text (" ⟶ " :: T.Text)

{- | 
The arrow representing a value or stuck term: 
it corresponds to Unicode character 219B (↛).
-}
lastStepArrow :: Doc ann
lastStepArrow = text (" ↛ " :: T.Text)

{- |
'prettyTerm' is used to prettyfy terms: it takes a 'Language.SimplyTyped.Syntax.Term' 
and returns a prettyfied version contained in a @'MonadReader' env@ monad.

The canonical choice for the monad is @'Language.SimplyTyped.Monad.Eval'@.
-}
prettyTerm :: forall m env ann.
              ( MonadReader env m
              , HasLocals env) 
           => Term          -- ^ Term to be prettyfied.
           -> m (Doc ann)   -- ^ The result is contained in a monad that implements 'MonadReader'.
prettyTerm = \case
    -- Boolean expressions
    LitTrue -> pure $ text "True"
    LitFalse -> pure $ text "False"
    IfThenElse cond t1 t2 -> (text "if" <+>) . align . sep <$> do
        ifChunck <- termParens cond
        thenChunck <- (text "then" <+>) <$> termParens t1
        elseChunck <- (text "else" <+>) <$> termParens t2
        pure [ifChunck, thenChunck, elseChunck]
    -- Arithmetic expressions
    LitZero -> pure $ text "0"
    Succ n -> (text "succ" <+>) <$> termParens n
    Prec n -> (text "prec" <+>) <$> termParens n
    IsZero n -> (text "isZero?" <+>) <$> termParens n
    -- Lambda abstractions, applications and variables
    Var k -> asks (getLocalVar k) >>= \case
        Just var -> pure $ pretty $ view nameL var
        Nothing  -> error "This cannot happen"
    Lam name typ body -> (lamText name typ <+>)
        <$> local (insertIntoLocals (name, typ)) (prettyTerm body)
    App t1 t2 -> align . sep <$> do
        func <- termParens t1
        arg <- termParens t2
        pure [func, arg]
  where
    lamText :: T.Text -> Typ -> Doc ann
    lamText name typ = 
        text "λ" <> pretty name <+> text ":" <+> pretty typ <> text "."
    -- | Surrounds the prettyfied version of a term with parentheses.
    termParens :: Term -> m (Doc ann)
    termParens t = case t of
        LitTrue -> prettyTerm t
        LitFalse -> prettyTerm t
        LitZero -> prettyTerm t
        Var _ -> prettyTerm t
        _     -> parens <$> prettyTerm t