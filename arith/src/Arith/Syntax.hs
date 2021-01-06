{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Arith.Syntax
    ( -- * Term
      Term(..)
    ) where

import Arith.Pretty ( text )
import Data.Text.Prettyprint.Doc
    ( (<+>), align, sep, parens, Pretty(pretty) )


-- | A term of the Arith language.
data Term 
    = LitTrue                     -- ^ True literal.
    | LitFalse                    -- ^ False literal.
    | LitZero                     -- ^ Zero literal.
    | Succ Term                   -- ^ Successor operator.
    | Prec Term                   -- ^ Predecessor operator.
    | IsZero Term                 -- ^ Checks if a term is zero.
    | IfThenElse Term Term Term   -- ^ If-Then-Else constructor.
    deriving (Eq, Show)

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