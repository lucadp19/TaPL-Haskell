{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
The "TyArith.Check" module implements the main typechecking function ('typeof')
plus some other auxiliary functions for types and type errors.
-}

module TyArith.Check 
    ( -- * Typechecker
      typeof
    , ( <?> )
      -- * Type Errors
      -- $typerr
    , TypeErr(..)
    ) where

import TyArith.Syntax ( Term(..) )
import TyArith.Types  ( Typ(..) )
import Control.Monad.Except ( MonadError(throwError) )

import Data.Text as T

import Data.Text.Prettyprint.Doc

{- | 
The static typechecker. 
It calculates the type of a given expression 
and it returns an error if the expression is not well typed.

The canonical choice for the monad is @'Either TypeErr'@.
-}
typeof :: (MonadError TypeErr m) 
       => Term
       -> m Typ
typeof = \case
    -- Literals
    LitTrue  -> pure Bool
    LitFalse -> pure Bool
    LitZero  -> pure Nat
    -- Arithmetic operators
    Succ n -> typeof n >>= \case
        Nat -> pure Nat
        _   -> throwError SuccNotNat
    Prec n -> typeof n >>= \case
        Nat -> pure Nat
        _   -> throwError PrecNotNat
    -- IsZero operator
    IsZero n -> typeof n >>= \case
        Nat -> pure Bool
        _   -> throwError IsZeroNotNat
    -- If-Then-Else
    IfThenElse cond thenTerm elseTerm -> typeof cond >>= \case
        Bool -> do
            thenTy <- typeof thenTerm
            elseTy <- typeof elseTerm
            if thenTy == elseTy 
                then pure thenTy
                else throwError DifferentIfBranchType
        _ -> throwError NonBooleanIfGuard

{- |
The @'(<?>)'@ operator is used to typecheck a given term 
and then apply an evaluation function on it. 

In particular, if the typechecker terminates without errors, it throws away the
type and evaluates the term with the given evaluation function; otherwise
it throws a 'TypeErr'.

It is canonically used with a @'Either' 'TypeErr'@ monad: 
it returns either a @Left err@ or the evaluation result wrapped in a @Right@
constructor.
-}
( <?> ) :: (MonadError TypeErr m)
        => (Term -> a) 
        -> Term 
        -> m a
f <?> t = typeof t >> pure (f t) 

{- $typerr
The process of typechecking might fail when a term is not well typed:
the 'typeof' function then returns an error of type 'TypeErr', which
represents all possible situations in which the typechecking might fail.

Type errors have their own 'Pretty' instance for pretty-printing.
-}

-- | Different static type errors.
data TypeErr
    = SuccNotNat
    | PrecNotNat
    | IsZeroNotNat
    | NonBooleanIfGuard
    | DifferentIfBranchType
    deriving (Eq, Show)

-- | 'Pretty' instance for 'TypeErr'.
instance Pretty TypeErr where
    pretty err = text "Type error:" <+> prettyErr err
      where
        prettyErr :: TypeErr -> Doc ann
        prettyErr = \case
            SuccNotNat -> text "succ must be applied to a value of type Nat."
            PrecNotNat -> text "prec must be applied to a value of type Nat."
            IsZeroNotNat -> text "\"isZero?\" must be applied to a value of type Nat."
            NonBooleanIfGuard -> text "\"if-then-else\" construct must have a guard of type Bool."
            DifferentIfBranchType -> text "\"if-then-else\" must have two branches with the same type."
        text :: T.Text -> Doc ann
        text = pretty