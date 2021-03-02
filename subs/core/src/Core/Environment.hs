{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-
The "Core.Environment" module contains two typeclasses:
- the first typeclass is called 'HasLocals' and it is used for all environments
containing local bindings;
- the second typeclass is called 'HasGlobals' and it's used for environments
with global bindings.
-}

module Core.Environment 
    ( -- * Local bindings
      HasLocals(..)
      -- * Global bindings
    , HasGlobals(..)
    ) where

import qualified Data.Text as T

{-|
The typeclass @HasLocals@ represents all data structures containing local bindings.
The typeclass gives access to three methods:

@
    insertIntoLocals :: bind -> env -> env
@ 

which inserts a new binding at the top of the binding list;

@
    getLocalIndex :: Text -> env -> Maybe Int
@ 

which returns the index of a local variable if found;

@
    getLocalBind :: Int -> env -> Maybe bind
@

which returns the name of the variable at the n-th position.
-}
class HasLocals env bind | env -> bind where
    {- | 
    Given a variable binding and an environment 
    it inserts the variable into the local environment.
    -}
    insertIntoLocals 
        :: bind         -- ^ The variable binding.
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
        -> Maybe bind

{-|
The typeclass @HasGlobals@ represents all data structures containing global bindings.
The typeclass gives access to two methods:

@
    insertIntoGlobals :: bind -> env -> env
@ 

which inserts a new global binding in the environment;

@
  getGlobalBind :: T.Text -> env -> Maybe bind
@ 

which returns the global variable binding with the given name, if found.
-}
class HasGlobals env bind | env -> bind where
    {- | 
    Given a global binding and an environment,
    it adds the new global variable into the environment. 
    -}
    insertIntoGlobals 
        :: bind         -- ^ The variable name with its bound expression.
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
        -> Maybe bind