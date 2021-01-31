{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
The "SimplyTyped.Types" module contains the types of this implementation 
of the simply typed lambda calculus (STLC).
-}

module SimplyTyped.Types 
    ( -- * Types
      -- $types
      Typ(..)
    ) where

import Data.Text as T
import Data.Text.Prettyprint.Doc

{- $types
In this implementations of STLC a 'Term' can be of type 'Nat' (for natural numbers),
'Bool' (for truth values) or an 'Arr' type, which is the type of functions between
two different types.

To pretty-print types, the 'Typ' datatype is made an instance of 'Pretty'.
-}

{- |
A Type of the TyArith language. 
It can be @Nat@, @Bool@ or an @Arr@ow type.
-}
data Typ
    = Bool          -- ^ The boolean type.
    | Nat           -- ^ THe natural numbers type.
    | Arr Typ Typ   -- ^ The arrow type.
    deriving (Eq, Show)

-- | The instance of 'Pretty' for a @Typ@.
instance Pretty Typ where
    pretty = \case
        Bool -> text "Bool"
        Nat  -> text "Nat"
        Arr t1 t2 -> pretty t1 <+> text "⟶ " <+> pretty t2
      where
        text :: T.Text -> Doc ann
        text = pretty