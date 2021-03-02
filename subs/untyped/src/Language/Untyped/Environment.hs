{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TypeFamilies #-}

{-
The "Language.Untyped.Environment" module contains the environment used
by this implementation of the Untyped Lambda Calculus.
In particular it contains local binding and global binding,
each with their auxiliary functions.
-}

module Language.Untyped.Environment 
    ( -- * Env type
      Env
      -- * Env functions
      -- ** Empty environment
    , emptyEnv
      -- ** Local bindings
    , HasLocals(..)
    , LocalBind
    , createLocalBind
      -- ** Global bindings
    , HasGlobals(..)
    , GlobalBind
    , createGlobalBind
    ) where

import qualified Data.Text as T
import Data.List ( elemIndex )

import Core.Environment ( HasGlobals(..), HasLocals(..) )
import Core.Lenses ( HasName(..), HasTerm(..) )

import Language.Untyped.Syntax ( Term )

import Lens.Micro ( Lens', lens, over, (^.) )
import Lens.Micro.Extras ( view )

{- |
The environment in which an expression is evaluated.
The environment consists of a list of local bindings and a list of global bindings.
-}
data Env = Env  { _locals :: [LocalBind]     -- ^ The local bindings of an expression.
                , _globals :: [GlobalBind]   -- ^ The global bindings.
                } deriving (Eq, Show)

-- | The empty environment, which is an environment without any binding.
emptyEnv :: Env
emptyEnv = Env { _locals = [], _globals = [] }

{- |
The @locals@ lens is used to modify the local environment.

==== __Examples__

To get the list of local bindings contained in an environment we can use 'view' or the '^.' operator:

@
view locals env === env ^. locals
@
-}
locals :: Lens' Env [LocalBind]
locals = lens _locals $ \env newBind -> env { _locals = newBind }

{- |
The @globals@ lens is used to modify the global environment.

==== __Examples__

To get the list of global bindings contained in an environment we can use 'view' or the '^.' operator:

@
view globals env === env ^. globals
@
-}
globals :: Lens' Env [GlobalBind]
globals = lens _globals $ \env newBind -> env { _globals = newBind } 

{- ------- Local and Global bindings ------- -}

-- | A single local binding, with the name of the bound variable and additional info.
newtype LocalBind = LocalBind { _localName :: T.Text }
    deriving (Eq, Show)

-- | Creates a new local binding with the given name.
createLocalBind :: T.Text -> LocalBind
createLocalBind name = LocalBind { _localName = name }

instance HasName LocalBind where
    type NameF LocalBind = T.Text
    nameL = lens _localName $ \binds newName -> binds { _localName = newName }

-- | The instance of @HasLocals@ for a simple list of @LocalBind@s.
instance HasLocals [LocalBind] LocalBind where

    -- 'insertIntoLocals' just inserts the new binding 
    -- at the beginning of the binding list
    insertIntoLocals = ( : ) 

    getLocalIndex var list = elemIndex var nameList
      where
        nameList :: [T.Text]
        nameList = view nameL <$> list

    getLocalBind ind list = list `getAt` ind
      where
        -- | Safe version of '(!!)'.
        getAt :: [a] -> Int -> Maybe a
        getAt [] _ = Nothing
        getAt (x:xs) n
            | n < 0     = Nothing
            | n == 0    = Just x
            | otherwise = xs `getAt` (n-1)

-- | The instance of @HasLocals@ for an environment containing local bindings.
instance HasLocals Env LocalBind where
    insertIntoLocals  = over locals . insertIntoLocals 
    getLocalIndex var = getLocalIndex var . view locals
    getLocalBind var  = getLocalBind var . view locals

{- ---- Global bindings ---- -}

-- | A single global binding, with the name of the bound variable, the expression bound to it and additional info.
data GlobalBind = GlobalBind 
    { _globalName :: T.Text
    , _globalTerm :: Term
    } deriving (Eq, Show)

-- | Creates a new global binding from the given name and term.
createGlobalBind :: T.Text -> Term -> GlobalBind
createGlobalBind name term = GlobalBind { _globalName = name, _globalTerm = term }

instance HasName GlobalBind where
    type NameF GlobalBind = T.Text
    nameL = lens _globalName $ \binds newName -> binds { _globalName = newName }

instance HasTerm GlobalBind where
    type TermF GlobalBind = Term
    termL = lens _globalTerm $ \binds newTerm -> binds { _globalTerm = newTerm }

-- | The instance of @HasGlobals@ for a simple list of @GlobalBind@s.
instance HasGlobals [GlobalBind] GlobalBind where
    -- 'insertIntoLocals' just inserts the new binding 
    -- at the beginning of the binding list
    insertIntoGlobals = ( : )
    
    getGlobalBind var list = go list 
      where
        go :: [GlobalBind] -> Maybe GlobalBind
        go [] = Nothing
        go (bind : binds)
            | bind^.nameL == var  = Just bind
            | otherwise           = go binds

-- | The instance of @HasGlobal@ for an environment containing global bindings.
instance HasGlobals Env GlobalBind where
    insertIntoGlobals = over globals . insertIntoGlobals
    getGlobalBind var = getGlobalBind var . view globals