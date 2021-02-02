{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
The "TyArith.Types" module contains the types of this implementation 
of the typed arithmetic language.
-} 
module TyArith.Types 
    ( Typ(..)
    ) where

import Data.Text.Prettyprint.Doc ( Pretty(pretty) )
import Data.Text as T

-- | A Type of the TyArith language. It can be either @Nat@ or @Bool@.
data Typ
    = Bool
    | Nat
    deriving (Eq, Show)

-- | 'Pretty' instance for 'Typ'.
instance Pretty Typ where
    pretty = \case
        Bool -> pretty ("Bool" :: T.Text)
        Nat  -> pretty ("Nat" :: T.Text)