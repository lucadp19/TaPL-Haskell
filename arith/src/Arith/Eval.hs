module Arith.Eval 
    ( step
    , eval
    ) where

import Arith.Syntax

-- | Returns whether a term is numeric or not.
-- A term is numeric if it's zero or the successor of a numeric term.
isNumeric :: Term -> Bool
isNumeric LitZero   = True
isNumeric (Succ a)  = isNumeric a
isNumeric _         = False

{-|
    Steps an expression into another expression. 
    If there is no applicable rule, it returns nothing.
-}
step :: Term -> Maybe Term
-- Boolean expressions
step (IfThenElse LitTrue a _)  = Just a
step (IfThenElse LitFalse _ b) = Just b
step (IfThenElse cond a b)     = IfThenElse <$> step cond <*> pure a <*> pure b
-- Arithmetic expressions
step (Succ n)           = Succ <$> step n
step (Prec (Succ n))          
    | isNumeric n       = Just n
    | otherwise         = Prec <$> step (Succ n)
step (IsZero LitZero)   = Just LitTrue
step (IsZero (Succ n))
    | isNumeric n       = Just LitFalse
    | otherwise         = IsZero <$> step (Succ n)
-- Stuck terms
step _ = Nothing

-- | Evaluates a term to a value or to a stuck term, repeatedly using the small-step semantic given. 
multistep :: Term -> Term
multistep t = case step t of
    Just t' -> multistep t'
    Nothing -> t

-- | Evaluates a term to a value or to a stuck term, repeatedly using the small-step semantic given.
eval :: Term -> Term
eval = multistep