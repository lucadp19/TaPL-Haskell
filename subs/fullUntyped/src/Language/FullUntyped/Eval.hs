{-# LANGUAGE LambdaCase #-}

{- |
The "Language.FullUntyped.Eval" module defines two evaluating functions:
the 'step' evaluator performs a single evaluation step, 
whereas the 'eval' function fully evaluates the expression.
-}

module Language.FullUntyped.Eval 
    ( -- * Evaluators
      -- ** Single step
      step
      -- ** Multistep
    , eval
    ) where

import Language.FullUntyped.Syntax ( Term(..) )

{- | 
Checks if a given term is numeric: a numeric term is either 
zero or the successor of a numeric term.
-}
isNumeric :: Term -> Bool
isNumeric LitZero = True 
isNumeric (Succ n) = isNumeric n
isNumeric _ = False

{- |
Describes which terms are values: a term is a value if it is semantically sound but
it cannot be further reduced.

Examples of values are literals, variables, numerical terms and lambda abstractions.
-}
isValue :: Term -> Bool 
isValue = \case
    LitTrue   -> True
    LitFalse  -> True
    LitZero   -> True
    Var _     -> True
    Succ n    -> isNumeric n
    Lam _ _   -> True
    _         -> False

-- | The shifting function: given an @Int@ and a @Term@ the function modifies the term shifting it into the right context.
shift :: Int -> Term -> Term
shift d = go 0
  where 
    go :: Int -> Term -> Term
    go c = \case
        -- The only interesting cases are Var and Lam
        Var k      -> Var $ if k >= c then k+d else k
        Lam name t -> Lam name $ go (c+1) t
        -- The rest is just propagation
        App t1 t2  -> App (go c t1) (go c t2)
        Succ t     -> Succ $ go c t
        Prec t     -> Prec $ go c t
        IsZero t   -> IsZero $ go c t
        IfThenElse cond t1 t2 ->
            IfThenElse (go c cond) (go c t1) (go c t2)
        -- Literals remain the same
        literal    -> literal

-- | The substitution function.
subst :: Int -> Term -> Term -> Term
subst j s = go 0
  where 
    go :: Int -> Term -> Term
    go c = \case
        Var k      -> if k == j+c then shift c s else Var k
        Lam name t -> Lam name $ go (c+1) t
        -- The rest is just propagation
        App t1 t2  -> App (go c t1) (go c t2)
        Succ t     -> Succ $ go c t
        Prec t     -> Prec $ go c t
        IsZero t   -> IsZero $ go c t
        IfThenElse cond t1 t2 ->
            IfThenElse (go c cond) (go c t1) (go c t2)
        -- Literals remain the same
        literal    -> literal

-- | The beta-reduction function: substitutes a @Term@ into the body of a lambda abstraction.
substTop :: Term    -- ^ The term to be substituted into the body of the lambda abstraction.
         -> Term    -- ^ The body of the lambda abstraction.
         -> Term
substTop s t = shift (-1) $ subst 0 (shift 1 s) t

{- | 
Steps an expression into another expression. 
If there is no applicable rule, it returns nothing.
-}
step :: Term -> Maybe Term
-- Boolean expressions
step (IfThenElse LitTrue a _) = Just a
step (IfThenElse LitFalse _ b) = Just b
step (IfThenElse cond a b) = IfThenElse <$> step cond <*> pure a <*> pure b
-- Arithmetic expressions
step (Succ n) = Succ <$> step n
step (Prec (Succ n))
    | isNumeric n = Just n
    | otherwise   = Prec <$> step (Succ n)
step (IsZero LitZero) = Just LitTrue
step (IsZero (Succ n))
    | isNumeric n = Just LitFalse
    | otherwise   = IsZero <$> step (Succ n)
-- Stepping function application
step (App (Lam name t1) t2) 
    | isValue t2 = pure $ substTop t2 t1
    | otherwise  = App (Lam name t1) <$> step t2
step (App t1 t2) = App <$> step t1 <*> pure t2
-- Stuck terms and values
step _           = Nothing

-- | Evaluates a term to a value or to a stuck term, repeatedly using the small-step semantic given. 
multistep :: Term -> Term
multistep t = case step t of
    Just t' -> multistep t'
    Nothing -> t

-- | Evaluates a term to a value or to a stuck term, repeatedly using the small-step semantic given. 
eval :: Term -> Term
eval = multistep

