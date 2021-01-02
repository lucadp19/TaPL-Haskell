{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Untyped.Monad where

import qualified Data.Text as T
import Untyped.Environment
import Control.Monad.Reader

-- | The Evaluation monad: it gives access to a lexical scope and the power to do IO.
newtype Eval a = Eval { runEval :: ReaderT Env IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader Env)
