module FullUntyped.Syntax where

import qualified Data.Text as T

-- | A Term of the Full Untyped Lambda Calculus.
data Term
    = LitTrue           -- ^ True literal.
    | LitFalse          -- ^ False literal.
    | LitZero           -- ^ Zero literal.
    | Succ Term         -- ^ The successor operator.
    | Prec Term         -- ^ The predecessor operator.
    | IsZero Term       -- ^ Checks if a given term is zero.
    | IfThenElse
        Term Term Term  -- ^ If-Then-Else construct.
    | Var Int           -- ^ A variable, represented by its De Bruijn index.
    | Lam T.Text Term   -- ^ A lambda abstraction with the parameter name.
    | App Term Term     -- ^ A function application.
    deriving (Eq, Show) 