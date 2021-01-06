{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Untyped.Monad 
    ( -- * The monad
      Eval(..)
    ) where

import Untyped.Environment ( Env )
import Control.Monad.Reader
import System.Console.Haskeline ( MonadException )

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
             , MonadException )
