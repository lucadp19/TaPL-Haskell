{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Arith.Pretty 
    ( prettyprint
    , text
    , evalArrow
    , stepArrow
    , lastStepArrow
    ) where

import Arith.Syntax
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

-- | The Pretty Term instance.
instance Pretty Term where
    pretty = \case
        LitTrue -> pretty True
        LitFalse -> pretty False
        LitZero -> pretty (0 :: Int)
        Succ t -> text "succ" <+> parens (pretty t)
        Prec t -> text "prec" <+> parens (pretty t)
        IsZero t -> text "isZero?" <+> parens (pretty t)
        IfThenElse cond e1 e2 -> 
            text "if" <+> align 
            ( sep [ parens (pretty cond)
                  , text "then" <+> parens (pretty e1)
                  , text "else" <+> parens (pretty e2) ] )