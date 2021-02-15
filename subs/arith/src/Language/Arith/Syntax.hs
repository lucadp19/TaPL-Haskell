{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Arith.Syntax
    ( -- * Term
      Term(..)
    ) where

import Language.Arith.Pretty ( text )
import Data.Text.Prettyprint.Doc


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
        Succ t -> text "succ" <+> termParens t
        Prec t -> text "prec" <+> termParens t
        IsZero t -> text "isZero?" <+> termParens t 
        IfThenElse cond thenT elseT -> 
            text "if" <+> align 
            ( sep [ termParens cond
                  , text "then" <+> termParens thenT
                  , text "else" <+> termParens elseT ] )
      where
        termParens :: Term -> Doc ann
        termParens t 
            | t `elem` [LitTrue, LitFalse, LitZero] = 
                pretty t
            | otherwise = parens $ pretty t