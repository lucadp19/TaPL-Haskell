{-# LANGUAGE GeneralisedNewtypeDeriving #-}

{- |
The "Language.Untyped.Monad" module defines the 'Eval' monad,
used to implement the lexical environment in which
terms are evaluated.
-}

module Language.FullUntyped.Monad 
    ( -- * The monad
      Eval(..)
    , CM.reduceEval
    ) where
  
import qualified Core.Monad as CM

import Language.FullUntyped.Environment ( Env )

{- |
The Evaluation monad: it gives access to a lexical scope based on the
environment type @Env@, and the power to do IO through @MonadIO@.
-}
type Eval = CM.Eval Env