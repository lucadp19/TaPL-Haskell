module Arith.Syntax where

-- | A term of the Arith language.
data Term = 
      LitTrue
    | LitFalse
    | LitZero
    | Succ Term
    | Prec Term
    | IsZero Term
    | IfThenElse Term Term Term
    deriving (Eq, Show)
