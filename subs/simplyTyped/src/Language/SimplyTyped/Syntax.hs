module Language.SimplyTyped.Syntax where

import Language.SimplyTyped.Types ( Typ )
import Data.Text as T

-- | A Term of the Simply Typed Lambda Calculus.
data Term
    = LitZero           -- ^ Zero literal.
    | LitTrue           -- ^ True literal.
    | LitFalse          -- ^ False literal.
    | Succ Term         -- ^ The successor operator.
    | Prec Term         -- ^ The predecessor operator.
    | IsZero Term       -- ^ Checks if a given term is zero.
    | IfThenElse 
        Term Term Term  -- ^ If-Then-Else construct.
    | Var Int           -- ^ A variable, represented by its De Bruijn index.
    | Lam 
        T.Text Typ Term -- ^ A lambda abstraction with the parameter name and type.
    | App Term Term     -- ^ A function application.
    deriving (Eq, Show)
