{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.FullUntyped.Pretty 
    ( -- * Main prettyfing function
      prettyTerm
      -- * Helpers
    , text
    , evalArrow
    , stepArrow
    , lastStepArrow
    ) where

import Language.FullUntyped.Syntax ( Term(..) )
import Language.FullUntyped.Environment
    ( HasLocals(insertIntoLocals, getLocalName) )

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Control.Monad.Reader


-- | Helper function for prettifying Text.
text :: T.Text -> Doc ann
text = pretty

-- | The arrow representing multistep evaluation.
evalArrow :: Doc ann
evalArrow = text (" ⇒ " :: T.Text)

-- | The arrow representing a single evaluation step.
stepArrow :: Doc ann
stepArrow = text (" ⟶ " :: T.Text)

-- | The arrow representing a value or stuck term.
lastStepArrow :: Doc ann
lastStepArrow = text (" ↛ " :: T.Text)

{- |
@prettyTerm@ takes a @Term@ and returns a pretty version contained in an 
environment based on a monad containing an environment @env@ with local variables.

The canonical choice for the monad is 'Language.FullUntyped.Monad.Eval'.
-}
prettyTerm :: forall m env ann.
              ( MonadReader env m
              , HasLocals env) 
           => Term 
           -> m (Doc ann)
prettyTerm = \case
    -- Boolean expressions
    LitTrue -> pure $ text "True"
    LitFalse -> pure $ text "False"
    IfThenElse cond t1 t2 -> (text "if" <+>) . align . sep <$> do
        ifChunck <- appParens cond
        thenChunck <- (text "then" <+>) <$> appParens t1
        elseChunck <- (text "else" <+>) <$> appParens t2
        pure [ifChunck, thenChunck, elseChunck]
    -- Arithmetic expressions
    LitZero -> pure $ text "0"
    Succ n -> (text "succ" <+>) <$> appParens n
    Prec n -> (text "prec" <+>) <$> appParens n
    IsZero n -> (text "isZero?" <+>) <$> appParens n
    -- Lambda abstractions, applications and variables
    Var k -> asks (getLocalName k) >>= \case
        Just name -> pure $ pretty name
        Nothing   -> error "This cannot happen"
    Lam name body -> (lamText name <+>)
        <$> local (insertIntoLocals name) (prettyTerm body)
    App t1 t2 -> align . sep <$> do
        func <- appParens t1
        arg <- appParens t2
        pure [func, arg]
  where
    lamText :: T.Text -> Doc ann
    lamText name = text "λ" <> pretty name <> text "."
    -- | Surrounds the prettyfied version of a term with parentheses.
    appParens :: Term -> m (Doc ann)
    appParens t = case t of
        LitTrue -> prettyTerm t
        LitFalse -> prettyTerm t
        LitZero -> prettyTerm t
        Var _ -> prettyTerm t
        _     -> parens <$> prettyTerm t