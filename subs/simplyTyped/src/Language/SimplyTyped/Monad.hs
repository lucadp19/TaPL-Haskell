{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{- |
The "Language.SimplyTyped.Monad" module defines the 'Eval' monad,
used to implement the lexical environment in which
terms are evaluated.
-}

module Language.SimplyTyped.Monad 
    ( -- * Eval monad
      Eval(..)
    ) where

import Language.SimplyTyped.Env ( Env )
import Control.Monad.Reader
import Control.Monad.Catch

{- |
The Evaluation monad: it gives access to a lexical scope based on the
environment type 'Env', and the power to do IO through 'MonadIO'.
-}
newtype Eval a = Eval { runEval :: ReaderT Env IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Env
             , MonadThrow
             , MonadCatch
             , MonadMask )