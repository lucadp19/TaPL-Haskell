{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Untyped.Pretty 
    ( -- * Main prettyfing function
      prettyEval
      -- * Helpers
    , text
    , evalArrow
    , stepArrow
    , lastStepArrow
    ) where

import Untyped.Syntax ( Term(..) )
import Untyped.Environment
import Untyped.Monad ( Eval )

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
@prettyEval@ takes a @Term@ and returns a prettyfied version contained in an 
environment based on the @Eval@ monad.
-}
prettyEval :: Term -> Eval (Doc ann)
prettyEval = \case
    Var k -> do 
        env <- ask
        case getLocalName k env of
            Just name -> pure $ pretty name
            Nothing   -> error "This cannot happen"
    Lam name body -> (lamText name <+>)
        <$> local (insertIntoLocals name) (prettyEval body)
    App t1 t2 -> (<+>) <$> appParens t1 <*> appParens t2
  where
    lamText :: T.Text -> Doc ann
    lamText name = text "λ" <> pretty name <> text "."

-- | Surrounds the prettyfied version of a term with parentheses.
appParens :: Term -> Eval (Doc ann)
appParens t = case t of
    Var _ -> prettyEval t
    _     -> parens <$> prettyEval t