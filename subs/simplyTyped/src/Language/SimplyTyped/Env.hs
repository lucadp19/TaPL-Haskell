{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-
The 'Language.SimplyTyped.Env' module contains the environment used
by this implementation of the Simply Typed Lambda Calculus.
In particular it contains local binding and global binding,
each with their own lenses and auxiliary functions.
-}

module Language.SimplyTyped.Env 
    ( -- * Env type
      Env
      -- * Lenses
      -- $lenses
      -- ** Environment lenses
    , locals, globals
      -- ** Classy-lenses
    , HasName(..)
    , HasType(..)
    , HasTerm(..)
      -- * Env functions
      -- ** Empty environment
    , emptyEnv
      -- ** Local bindings
    , LocalBind
    , HasLocals
    , insertIntoLocals, getLocalIndex, getLocalVar
      -- ** Global bindings
    , GlobalBind
    , HasGlobals
    , insertIntoGlobals, getGlobalTerm
    ) where

import qualified Data.Text as T
import Data.List ( elemIndex )

import Language.SimplyTyped.Types ( Typ )
import Language.SimplyTyped.Syntax ( Term )

import Lens.Micro ( (^.), lens, over, Lens' )
import Lens.Micro.Extras ( view )

{- |
The environment in which an expression is evaluated.
The environment consists of a list of local bindings and a list of global bindings.
-}
data Env = Env  
    { _locals :: [LocalBind]     -- ^ The local bindings of an expression.
    , _globals :: [GlobalBind]   -- ^ The global bindings.
    } deriving (Eq, Show)

-- | A single local binding, with the name of the bound variable and its type.
data LocalBind = LocalBind 
    { _localName :: T.Text
    , _localType :: Typ
    } deriving (Eq, Show)
-- | A single global binding, with the name of the bound variable, the expression bound to it and its type.
data GlobalBind = GlobalBind
    { _globalName :: T.Text
    , _globalType :: Typ
    , _globalTerm :: Term
    } deriving (Eq, Show)

{- $lenses
To deal with the nested record structure the module implements several 'Lens'
used to modify or view the various field records.
-}

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

{- |
The @HasName@ class is the typeclass of all records containing a "name" field.
It contains a single lens, called @nameL@, used to modify the name field.
-}
class HasName a where
    nameL :: Lens' a T.Text

instance HasName LocalBind where
    nameL = lens _localName $ \binds newName -> binds { _localName = newName }
instance HasName GlobalBind where
    nameL = lens _globalName $ \binds newName -> binds { _globalName = newName }

{- |
The @HasType@ class is the typeclass of all records containing a "type" field.
It contains a single lens, called @typeL@, used to modify the type field.
-}
class HasType a where
    typeL :: Lens' a Typ

instance HasType LocalBind where
    typeL = lens _localType $ \binds newType -> binds { _localType = newType }
instance HasType GlobalBind where
    typeL = lens _globalType $ \binds newType -> binds { _globalType = newType }

{- |
The @HasTerm@ class is the typeclass of all records containing a "term" field.
It contains a single lens, called @termL@, used to modify the term field.
-}
class HasTerm a where
    termL :: Lens' a Term

instance HasTerm GlobalBind where
    termL = lens _globalTerm $ \binds newTerm -> binds { _globalTerm = newTerm }


-- | The empty environment, which is an environment without any binding.
emptyEnv :: Env
emptyEnv = Env { _locals = [], _globals = [] }

{- |
The typeclass @'HasLocals'@ represents all data structures containing local bindings.
In particular it gives access to three methods:

@
    'insertIntoLocals' :: ('T.Text', 'Language.SimplyTyped.Typ') -> a -> a
@ 

which inserts a new variable at the top of the binding list;

@
    'getLocalIndex' :: 'T.Text' -> a -> 'Maybe' 'Int'
@ 

which returns the index of a local variable if found;

@
    'getLocalVar' :: 'Int' -> a -> 'LocalBind'
@

which returns the name and type of the variable at the n-th position.
-}
class HasLocals a where
    -- | Given a variable and an environment it inserts the variable into the local environment.
    insertIntoLocals :: (T.Text, Typ)  -- ^ The variable name and type.
                     -> a              -- ^ The given environment.
                     -> a
    -- | Given a variable name and an environment it returns the De Bruijn index of the local variable, if found.
    getLocalIndex :: T.Text     -- ^ The variable name. 
                  -> a          -- ^ The given environment.
                  -> Maybe Int
    -- | Given a De Brujin index and an environment it returns the corresponding name and type of the variable.
    getLocalVar :: Int         -- ^ The De Brujin index.
                -> a           -- ^ The given environment.
                -> Maybe LocalBind

-- | The instance of @HasLocals@ for a simple list of @LocalBind@s.
instance HasLocals [LocalBind] where
    -- Creates the new LocalBind and puts it at the top of the list
    insertIntoLocals (name, typ) list = 
        LocalBind { _localName = name, _localType = typ } : list
    getLocalIndex name list = elemIndex name (view nameL <$> list)
    getLocalVar idx list = list `getAt` idx
      where
        -- | Safe version of '(!!)'.
        getAt :: [a] -> Int -> Maybe a
        getAt [] _ = Nothing
        getAt (x:xs) n
            | n < 0     = Nothing
            | n == 0    = Just x
            | otherwise = xs `getAt` (n-1)

-- | The instance of @HasLocals@ for an environment containing local bindings.
instance HasLocals Env where
    insertIntoLocals (n, t) = over locals $ insertIntoLocals (n, t)
    getLocalIndex varName env = getLocalIndex varName $ view locals env
    getLocalVar idx env = getLocalVar idx $ view locals env

{-|
The typeclass @HasGlobals@ represents all data structures containing global bindings.
In particular it gives access to these methods:

@
    'insertIntoGlobals' :: ('T.Text', 'Term') -> a -> a
@ 

which inserts a new variable with its bound value at the top of the binding list;

@
  'getGlobalTerm' :: 'T.Text' -> a -> 'Maybe' 'Term'
@ 

which returns the term bound to a global variable if found.
-}
class HasGlobals a where
    {- | 
    Given a pair @('T.Text', 'Term')@ representing the name of a new global variable 
    and the expression bound to it and an environment,
    it adds the new global variable into the environment.
    -}
    insertIntoGlobals :: (T.Text, Typ, Term)  -- ^ The variable name, type and its bound expression.
                      -> a                    -- ^ The given environment.
                      -> a
    -- | Given a variable name and an environment it returns the term bound to the global variable, if found.
    getGlobalTerm :: T.Text     -- ^ The variable name. 
                  -> a          -- ^ The given environment.
                  -> Maybe Term

-- | The instance of @HasGlobals@ for a simple list of @'GlobalBind'@s.
instance HasGlobals [GlobalBind] where
    insertIntoGlobals (name, typ, term) list =
        GlobalBind { _globalName = name
                   , _globalType = typ
                   , _globalTerm = term } : list
    getGlobalTerm name = go
      where
        go :: [GlobalBind] -> Maybe Term
        go [] = Nothing
        go (bind:xs)
            | bind^.nameL == name = Just $ bind^.termL
            | otherwise           = go xs

-- | The instance of @HasGlobal@ for an environment containing global bindings.
instance HasGlobals Env where
    insertIntoGlobals var = over globals (insertIntoGlobals var)
    getGlobalTerm name env = getGlobalTerm name $ view globals env
