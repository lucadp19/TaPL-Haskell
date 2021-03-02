{- |
The "Core.Pretty" module contains some helper functions for pretty-printing
terms and REPL results.
-}

module Core.Pretty 
    ( -- * Helpers
      text
    , precParens
      -- ** Arrows
    , evalArrow
    , stepArrow
    , lastStepArrow
    ) where


import qualified Data.Text as T
import Data.Text.Prettyprint.Doc ( Doc, Pretty(pretty), parens )

-- | Helper function for pretty-printing Text.
text :: T.Text -> Doc ann
text = pretty

{- | 
Modifies a @'Doc' ann@ surrounding it with parenthesis if the condition is true,
leaves it unaltered otherwise.
-}
precParens :: Bool      -- ^ The condition.
           -> Doc ann   -- ^ The given pretty term.
           -> Doc ann   -- ^ The resulting pretty term.
precParens cond
    | cond      = parens
    | otherwise = id

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