{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{- |
The "Core.Monad" module defines the 'Eval' monad,
used to implement the lexical environment in which
terms are evaluated in the various languages.
-}

module Core.Monad 
    ( -- * Monad
      Eval (..)
      -- * Helper functions
    , reduceEval
    ) where

import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Reader ( MonadReader, ReaderT(..) )
import Control.Monad.Catch ( MonadCatch, MonadMask, MonadThrow )

{- |
The Evaluation monad: it gives access to a lexical scope based on the
environment type @env@, and the power to do IO through 'MonadIO'.

It's also an instance of 'MonadThrow'/'MonadCatch'/'MonadMask' to be used with
the "System.Console.Haskeline" package to implement a REPL.

One way to use it is to declare a type synonym with the given environment:
for example if @Env@ is the type of the environment it could be useful to define

@
    type Eval' = Eval Env
@

so that the environment type can be omitted.
-}
newtype Eval env a = Eval { runEval :: ReaderT env IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader env
             , MonadThrow
             , MonadCatch
             , MonadMask )

{- | 
The 'reduceEval' function takes an environment and a term of type 
@'EvalEnv' env a@ and returns the evaluated contents of the monad.
-}
reduceEval :: env         -- ^ Environment in which the monad has to be evaluated.
           -> Eval env a  -- ^ The monad.
           -> IO a
reduceEval env eval = runReaderT (runEval eval) env