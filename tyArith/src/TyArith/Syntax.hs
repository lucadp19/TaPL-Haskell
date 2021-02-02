{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
The "TyArith.Syntax" module contains the syntactic terms of this implementation 
of the typed arithmetic language.
-} 

module TyArith.Syntax where

import Data.Text.Prettyprint.Doc
import Data.Text as T

-- | A Term of the Typed Arith language.
data Term
    = LitZero           -- ^ Zero literal.
    | LitTrue           -- ^ True literal.
    | LitFalse          -- ^ False literal.
    | Succ Term         -- ^ The successor operator.
    | Prec Term         -- ^ The predecessor operator.
    | IsZero Term       -- ^ Checks if a given term is zero.
    | IfThenElse 
        Term Term Term  -- ^ If-Then-Else construct.
    deriving (Eq, Show)

-- | 'Pretty' instance for 'Term'.
instance Pretty Term where
    pretty = \case
        LitZero -> text "0"
        LitTrue -> text "True"
        LitFalse -> text "False"
        Succ n -> text "succ" <+> termParens n
        Prec n -> text "prec" <+> termParens n
        IsZero t -> text "isZero?" <+> termParens t
        IfThenElse cond e1 e2 -> text "if" <+> align
            ( sep [ termParens cond
                  , text "then" <+> termParens e1
                  , text "else" <+> termParens e2 ] )
      where
        -- | Helper function for pretty-printing 'T.Text'.
        text :: T.Text -> Doc ann
        text = pretty
        -- | Puts parentheses where they are needed.
        termParens :: Term -> Doc ann
        termParens = \case
            LitTrue -> pretty LitTrue
            LitFalse -> pretty LitFalse
            LitZero -> pretty LitZero
            term -> parens $ pretty term
