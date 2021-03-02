{-# LANGUAGE TypeFamilies #-}

{- |
The "Core.Lenses" module defines some lenses that can be useful
when working with environments and records.
-}

module Core.Lenses 
    ( -- * Lenses
      -- $lenses
      -- ** Classy-lenses for fields
      HasName(..)
    , HasType(..)
    , HasTerm(..)
    ) where

import qualified Data.Text as T
import Data.List ( elemIndex )

import Lens.Micro ( Lens' )

{- $lenses
To deal with the nested record structure the module implements several @'Lens.Micro.Lens'@es
used to modify or view the various field records.
-}

{- |
The @HasName@ class is the typeclass of all records containing a "name" field.
It contains a single lens, called @nameL@, used to modify the name field, and
a type family @NameF@ to specify the type of the field.
-}
class HasName a where
    -- | Type family synonym for the type of the "name" field.
    type NameF a :: *
    -- | Lens to get/set the "name" field.
    nameL :: Lens' a (NameF a)

{- |
The @HasType@ class is the typeclass of all records containing a "type" field.
It contains a single lens, called @typeL@, used to modify the type field, and
a type family @TypeF@ to specify the type of the field.
-}
class HasType a where
    -- | Type family synonym for the type of the "type" field.
    type TypeF a :: *
    -- | Lens to get/set the "type" field.
    typeL :: Lens' a (TypeF a)

{- |
The @HasTerm@ class is the typeclass of all records containing a "term" field.
It contains a single lens, called @termL@, used to modify the term field, and
a type family @TermF@ to specify the type of the field.
-}
class HasTerm a where
    -- | Type family synonym for the type of the "term" field.
    type TermF a :: *
    -- | Lens to get/set the "term" field.
    termL :: Lens' a (TermF a)
