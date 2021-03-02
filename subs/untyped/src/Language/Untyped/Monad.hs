{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{- |
The "Language.Untyped.Monad" module defines the 'Eval' monad,
used to implement the lexical environment in which
terms are evaluated.
-}

module Language.Untyped.Monad 
    ( -- * The monad
      Eval(..),
      CM.reduceEval
    ) where

import qualified Core.Monad as CM

import Language.Untyped.Environment ( Env )

-- | Type synonym for the @Eval@ monad.
type Eval = CM.Eval Env