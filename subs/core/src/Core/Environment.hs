{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StarIsType #-}

{-
The "Core.Environment" module contains two typeclasses:
- the first typeclass is called 'HasLocals' and it is used for all environments
containing local bindings;
- the second typeclass is called 'HasGlobals' and it's used for environments
with global bindings.
-}

module Core.Environment 
    ( -- * Local bindings
      HasLocals
    , insertIntoLocals, getLocalIndex, getLocalBind
      -- * Global bindings
    , HasGlobals
    , insertIntoGlobals, getGlobalBind, 
    ) where

import qualified Data.Text as T

{-|
The typeclass @HasLocals@ represents all data structures containing local bindings.

First of all it defines a type synonym, @LBind@, which represents 
the type of a single local binding in the environment.

Then the typeclass gives access to three methods:

@
    insertIntoLocals :: LBind env -> env -> env
@ 

which inserts a new binding at the top of the binding list;

@
    getLocalIndex :: Text -> env -> Maybe Int
@ 

which returns the index of a local variable if found;

@
    getLocalBind :: Int -> env -> Maybe (LBind env)
@

which returns the name of the variable at the n-th position.
-}
class HasLocals env where
    -- | Type synonym that represents a local binding.
    type LBind env :: *
    {- | 
    Given a variable binding and an environment 
    it inserts the variable into the local environment.
    -}
    insertIntoLocals 
        :: LBind env    -- ^ The variable binding.
        -> env          -- ^ The given environment.
        -> env

    {- |
    Given a variable name and an environment it returns 
    the De Bruijn index of the local variable if found, 
    @'Nothing'@ otherwise. 
    -}
    getLocalIndex 
        :: T.Text       -- ^ The variable name. 
        -> env          -- ^ The given environment.
        -> Maybe Int

    {- | 
    Given a De Brujin index and an environment it returns
    the corresponding variable binding if found, 
    @'Nothing'@ otherwise.
    -}
    getLocalBind 
        :: Int       -- ^ The De Brujin index.
        -> env       -- ^ The given environment.
        -> Maybe (LBind env)

{-|
The typeclass @HasGlobals@ represents all data structures containing global bindings.

First of all it defines a type synonym, @GBind@, which represents 
the type of a single global binding in the environment.

Then the typeclass gives access to two methods:

@
    insertIntoGlobals :: GBind env -> env -> env
@ 

which inserts a new global binding in the environment;

@
  getGlobalBind :: T.Text -> env -> Maybe (GBind env)
@ 

which returns the global variable binding with the given name, if found.
-}
class HasGlobals env where
    -- | Type synonym that represents a global binding.
    type GBind env :: *
    {- | 
    Given a global binding and an environment,
    it adds the new global variable into the environment. 
    -}
    insertIntoGlobals 
        :: GBind env    -- ^ The variable name with its bound expression.
        -> env          -- ^ The given environment.
        -> env
    {- | 
    Given a variable name and an environment it returns
    the corresponding global variable bind if found,
    @'Nothing'@ otherwise. 
    -}
    getGlobalBind 
        :: T.Text       -- ^ The variable name. 
        -> env          -- ^ The given environment.
        -> Maybe (GBind env)