{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
The "Language.TyArith.Eval" module defines two evaluating functions:
the 'step' evaluator performs a single evaluation step, 
whereas the 'eval' function fully evaluates the expression.
-}

module Language.TyArith.Eval 
    ( -- * Evaluators
      -- ** Single step
      step
      -- ** Multistep
    , eval ) where

import Language.TyArith.Syntax ( Term(..) )

{-
Auxiliary function that decides if a term is numeric 
(which means 'Zero' or @'Succ'@ of a numerical term). 
-}
isNumerical :: Term -> Bool
isNumerical LitZero  = True
isNumerical (Succ n) = isNumerical n
isNumerical _        = False

{- | 
Steps an expression into another expression. 
If there is no applicable rule, it returns nothing.
-}
step :: Term -> Maybe Term
-- Arithmetic expressions
step (Succ n)
    | isNumerical n = Nothing 
    | otherwise     = Succ <$> step n
step (Prec (Succ n))
    | isNumerical n = Just n
    | otherwise     = Nothing
step (Prec LitZero) = Just LitZero
-- IsZero operator
step (IsZero LitZero) = Just LitTrue
step (IsZero (Succ n))
    | isNumerical n   = Just LitFalse
    | otherwise       = IsZero <$> step (Succ n)
-- Boolean expressions
step (IfThenElse LitTrue thenT _)  = Just thenT
step (IfThenElse LitFalse _ elseT) = Just elseT
step (IfThenElse cond thenT elseT) = IfThenElse <$> step cond <*> pure thenT <*> pure elseT
-- Stuck terms and values
step _ = Nothing

-- | Evaluates a term to a value or to a stuck term, repeatedly using the small-step semantic given. 
multistep :: Term -> Term
multistep t = case step t of
    Just t' -> multistep t'
    Nothing -> t

-- | Evaluates a term to a value or to a stuck term, repeatedly using the small-step semantic given. 
eval :: Term -> Term
eval = multistep