{-# LANGUAGE OverloadedStrings #-}

{- |
The "Language.Arith.Pretty" module contains some helper functions for pretty-printing
terms and REPL results.
-}

module Language.Arith.Pretty 
    ( -- * Helpers
      text
      -- ** Arrows
    , evalArrow
    , stepArrow
    , lastStepArrow
    ) where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc ( Doc, Pretty(pretty) )

-- | Helper function for prettifying Text.
text :: T.Text -> Doc ann
text = pretty

{- | 
The arrow representing multistep evaluation:
it corresponds to Unicode character 21D2 (⇒).
-}
evalArrow :: Doc ann
evalArrow = text " ⇒ "

{- |
The arrow representing a single evaluation step:
it corresponds to Unicode character 2192 (→).
-}
stepArrow :: Doc ann
stepArrow = text " ⟶ "

{- | 
The arrow representing a value or stuck term: 
it corresponds to Unicode character 219B (↛).
-}
lastStepArrow :: Doc ann
lastStepArrow = text " ↛ "