{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Language.FullUntyped.Monad 
    ( -- * The monad
      Eval(..)
    ) where

import Language.FullUntyped.Environment ( Env )
import Control.Monad.Reader
import Control.Monad.Catch

{- |
The Evaluation monad: it gives access to a lexical scope based on the
environment type @Env@, and the power to do IO through @MonadIO@.
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