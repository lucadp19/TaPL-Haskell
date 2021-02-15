module Language.Untyped.Syntax where

import qualified Data.Text as T

-- | A Term of the Untyped Lambda Calculus.
data Term
    = Var Int           -- ^ A variable, represented by its De Bruijn index.
    | Lam T.Text Term   -- ^ A lambda abstraction with the parameter name.
    | App Term Term     -- ^ A function application.
    deriving (Eq, Show) 