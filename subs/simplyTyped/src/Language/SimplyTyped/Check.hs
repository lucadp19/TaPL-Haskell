{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
The "Language.SimplyTyped.Check" module implements the main typechecking function ('typeof')
plus some other auxiliary functions for types and type errors.
-}

module Language.SimplyTyped.Check 
    ( -- * Typechecker
      typeof
    , ( <?> )
      -- * Type Errors
      -- $typerr
    , TypeErr(..)
    )where

import Language.SimplyTyped.Syntax ( Term(..) )
import Language.SimplyTyped.Types  ( Typ(..) )
import Language.SimplyTyped.Env

import Control.Monad.Reader ( MonadReader(local), asks )
import Control.Monad.Except ( MonadError(throwError) )

import Lens.Micro.Extras ( view )

import Data.Text as T
import Data.Text.Prettyprint.Doc

{- | 
The static typechecker. 
It calculates the type of a given expression in an environment
(given by the 'MonadReader' constraint)
and it returns an error if the expression is not well typed.

The usual choice for the monad is @'Control.Monad.Except.ExceptT' 'TypeErr' 'Language.SimplyTyped.Eval'@.
-}
typeof :: ( MonadError TypeErr m
          , MonadReader env m
          , HasLocals env ) 
       => Term  -- ^ The term to be typed.
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
    -- Variable
    Var k -> do
        var <- asks $ getLocalVar k
        case var of
            Nothing -> throwError VariableOutOfBounds
            Just bind -> pure $ view typeL bind -- gets the type from the given bind
    -- Lambda abstraction
    Lam var ty body -> Arr ty <$> local (insertIntoLocals (var, ty)) (typeof body)
    -- Function application
    App fun expr -> typeof fun >>= \case
        Arr t1 t2 -> do
            ty <- typeof expr
            if ty == t1
                then pure t2
                else throwError WrongParamType
        _ -> throwError NonFunctionalApp

{- |
The @'(<?>)'@ operator is used to typecheck a given term 
and then apply an evaluation function on it. 

In particular, if the typechecker terminates without errors, it throws away the
type and evaluates the term with the given evaluation function; otherwise
it throws a 'TypeErr'.

It is canonically used with a 
@'Control.Monad.Except.ExceptT' 'TypeErr' 'Language.SimplyTyped.Eval'@ monad: 
when unwrapped it returns either a @Left err@ or the evaluation result wrapped in a @Right@
constructor.
-}
( <?> ) :: ( MonadError TypeErr m
           , MonadReader env m
           , HasLocals env )
        => (Term -> a) -- ^ The evaluation function to be used.
        -> Term        -- ^ The term to be typechecked and then evaluated.
        -> m a
f <?> t = typeof t >> pure (f t) 

{- $typerr
The process of typechecking might fail when a term is not well typed:
the 'typeof' function then returns an error of type 'TypeErr', which
represents all possible situations in which the typechecking might fail.

Type errors have their own 'Pretty' instance for pretty-printing.
-}

-- | A @'TypeErr'@ represents a possible type error.
data TypeErr
    = SuccNotNat                -- ^ Successor applied to a non-'Nat' value.
    | PrecNotNat                -- ^ Predecessor applied to a non-'Nat' value.
    | IsZeroNotNat              -- ^ IsZero applied to a non-'Nat' value.
    | NonBooleanIfGuard         -- ^ If guard has a non-'Bool' type.
    | DifferentIfBranchType     -- ^ If branches have different types.
    | VariableOutOfBounds       -- ^ Variable is not present in the environment.
    | WrongParamType            -- ^ The argument to a function has the wrong type.
    | NonFunctionalApp          -- ^ A function application without a function.
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
            VariableOutOfBounds -> text "Variable number is out of bounds in the given context."
            WrongParamType -> text "A function application must have the correct parameter type."
            NonFunctionalApp -> text "A function application must take a function."
        text :: T.Text -> Doc ann
        text = pretty