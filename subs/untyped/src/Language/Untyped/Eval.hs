{-# LANGUAGE LambdaCase #-}

{- |
The "Language.Untyped.Eval" module defines two evaluating functions:
the 'step' evaluator performs a single evaluation step, 
whereas the 'eval' function fully evaluates the expression.
-}

module Language.Untyped.Eval 
    ( -- * Evaluators
      -- ** Single step
      step
      -- ** Multistep
    , eval
    ) where

import Language.Untyped.Syntax ( Term(..) )


-- | Checks if a given term is a value: a term is a value if and only if it's a single lambda abstraction.
isValue :: Term -> Bool
isValue = \case
    Lam _ _ -> True
    _       -> False

-- | The shifting function: given an @'Int'@ and a @'Term'@ the function modifies the term shifting it into the right context.
shift :: Int -> Term -> Term
shift d = go 0
  where 
    go :: Int -> Term -> Term
    go c = \case
        Var k      -> Var $ if k >= c then k+d else k
        Lam name t -> Lam name $ go (c+1) t
        App t1 t2  -> App (go c t1) (go c t2) 

-- | The substitution function.
subst :: Int -> Term -> Term -> Term
subst j s = go 0
  where 
    go :: Int -> Term -> Term
    go c = \case
        Var k      -> if k == j+c then shift c s else Var k
        Lam name t -> Lam name $ go (c+1) t
        App t1 t2  -> App (go c t1) (go c t2) 

-- | The beta-reduction function: substitutes a @'Term'@ into the body of a lambda abstraction.
substTop :: Term    -- ^ The term to be substituted into the body of the lambda abstraction.
         -> Term    -- ^ The body of the lambda abstraction.
         -> Term
substTop s t = shift (-1) $ subst 0 (shift 1 s) t

{- | 
Steps an expression into another expression. 
If there is no applicable rule, it returns nothing.
-}
step :: Term -> Maybe Term
step (App (Lam name t1) t2) 
    | isValue t2 = pure $ substTop t2 t1
    | otherwise  = App (Lam name t1) <$> step t2
step (App t1 t2) = App <$> step t1 <*> pure t2
step _           = Nothing

-- | Evaluates a term to a value or to a stuck term, repeatedly using the small-step semantic given. 
multistep :: Term -> Term
multistep t = case step t of
    Just t' -> multistep t'
    Nothing -> t

-- | Evaluates a term to a value or to a stuck term, repeatedly using the small-step semantic given. 
eval :: Term -> Term
eval = multistep

