{-# LANGUAGE OverloadedStrings #-}

module Arith.Pretty 
    ( -- * Helpers
      prettyprint
    , text
    , evalArrow
    , stepArrow
    , lastStepArrow
    ) where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc

-- | Helper function for prettifying Text.
text :: T.Text -> Doc ann
text = pretty

-- | The arrow representing multistep evaluation.
evalArrow :: Doc ann
evalArrow = text (" ⇒ " :: T.Text)

-- | The arrow representing a single evaluation step.
stepArrow :: Doc ann
stepArrow = text (" ⟶ " :: T.Text)

-- | The arrow representing a value or stuck term.
lastStepArrow :: Doc ann
lastStepArrow = text (" ↛ " :: T.Text)

-- | Alias for prettifying and then printing something.
prettyprint :: Pretty a => a -> IO ()  
prettyprint = print . pretty