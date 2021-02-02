{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Untyped.Pretty 
    ( -- * Main prettyfing function
      prettyTerm
      -- * Helpers
    , text
      -- ** Arrows
    , evalArrow
    , stepArrow
    , lastStepArrow
    ) where

import Untyped.Syntax ( Term(..) )
import Untyped.Environment
    ( HasLocals(insertIntoLocals, getLocalName) )

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Control.Monad.Reader ( MonadReader(local), asks )


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
@'prettyTerm'@ takes a @'Term'@ and returns a prettyfied version contained in a 
monadic environment containing local variables.

The canonical choice for the monad is @'Untyped.Monad.Eval'@.
-}
prettyTerm :: ( MonadReader env m
              , HasLocals env )
           => Term 
           -> m (Doc ann)
prettyTerm = \case
    Var k -> asks (getLocalName k) >>= \case
        Just name -> pure $ pretty name
        Nothing   -> error "This cannot happen"
    Lam name body -> (lamText name <+>)
        <$> local (insertIntoLocals name) (prettyTerm body)
    App t1 t2 -> (<+>) <$> appParens t1 <*> appParens t2
  where
    lamText :: T.Text -> Doc ann
    lamText name = text "λ" <> pretty name <> text "."

-- | Surrounds the prettyfied version of a term with parentheses.
appParens :: ( MonadReader env m
             , HasLocals env )
          => Term 
          -> m (Doc ann)
appParens t = case t of
    Var _ -> prettyTerm t
    _     -> parens <$> prettyTerm t