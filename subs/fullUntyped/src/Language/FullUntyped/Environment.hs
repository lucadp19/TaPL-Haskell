{-# LANGUAGE FlexibleInstances #-}
module Language.FullUntyped.Environment 
    ( -- * Env type
      Env
      -- * Env functions
      -- ** Empty environment
    , emptyEnv
      -- ** Local bindings
    , HasLocals
    , insertIntoLocals, getLocalIndex, getLocalName
      -- ** Global bindings
    , HasGlobals
    ,  insertIntoGlobals, getGlobalTerm, 
    ) where

import qualified Data.Text as T
import Data.List ( elemIndex )

import Language.FullUntyped.Syntax ( Term )

{- |
The environment in which an expression is evaluated.
The environment consists of a list of local bindings and a list of global bindings.
-}
data Env = Env  { locals :: [LocalBind]     -- ^ The local bindings of an expression.
                , globals :: [GlobalBind]   -- ^ The global bindings.
                } deriving (Eq, Show)

-- Still haven't understood what it's for
data Binding = NameBinding
    deriving (Eq, Show)
-- | A single local binding, with the name of the bound variable and additional info.
type LocalBind = (T.Text, Binding)
-- | A single global binding, with the name of the bound variable, the expression bound to it and additional info.
type GlobalBind = (T.Text, Binding, Term)

-- | The empty environment, which is an environment without any binding.
emptyEnv :: Env
emptyEnv = Env { locals = [], globals = [] }

{-|
The typeclass @HasLocals@ represents all data structures containing local bindings.
In particular it gives access to three methods:

@
    insertIntoLocals :: Text -> a -> a
@ 

which inserts a new variable at the top of the binding list;

@
    getLocalIndex :: Text -> a -> Maybe Int
@ 

which returns the index of a local variable if found;

@
    getLocalName :: Int -> a -> Text
@

which returns the name of the variable at the n-th position.
-}
class HasLocals a where
    -- | Given a variable name and an environment it inserts the variable into the local environment.
    insertIntoLocals :: T.Text  -- ^ The variable name.
                     -> a        -- ^ The given environment.
                     -> a
    -- | Given a variable name and an environment it returns the De Bruijn index of the local variable, if found.
    getLocalIndex :: T.Text     -- ^ The variable name. 
                  -> a          -- ^ The given environment.
                  -> Maybe Int
    -- | Given a De Brujin index and an environment it returns the corresponding name of the variable.
    getLocalName :: Int         -- ^ The De Brujin index.
                 -> a           -- ^ The given environment.
                 -> Maybe T.Text

-- | The instance of @HasLocals@ for a simple list of @LocalBind@s.
instance HasLocals [LocalBind] where
    insertIntoLocals var list = (var, NameBinding) : list
    getLocalIndex var list = elemIndex var (fst <$> list)
    getLocalName ind list = (fst <$> list) `getAt` ind
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
    insertIntoLocals var env = env { locals = insertIntoLocals var $ locals env }
    getLocalIndex    var env = getLocalIndex var $ locals env
    getLocalName     var env = getLocalName  var $ locals env

{-|
The typeclass @HasGlobals@ represents all data structures containing global bindings.
In particular it gives access to these methods:

@
    insertIntoGlobals :: (Text, Term) -> a -> a
@ 

which inserts a new variable with its bound value at the top of the binding list;

@
  getGlobalTerm :: T.Text -> a -> Maybe Term
@ 

which returns the term bound to a global variable if found.
-}
class HasGlobals a where
    -- | Given a pair @(Text, Term)@ representing the name of a new global variable and the expression bound to it and an environment,
    -- it adds the new global variable into the environment.
    insertIntoGlobals :: (T.Text, Term)  -- ^ The variable name with its bound expression.
                      -> a               -- ^ The given environment.
                      -> a
    -- | Given a variable name and an environment it returns the term bound to the global variable, if found.
    getGlobalTerm :: T.Text     -- ^ The variable name. 
                  -> a          -- ^ The given environment.
                  -> Maybe Term

-- | The instance of @HasGlobals@ for a simple list of @GlobalBind@s.
instance HasGlobals [GlobalBind] where
    insertIntoGlobals (var, expr) list = (var, NameBinding, expr) : list
    getGlobalTerm var list = go list 
      where
        go :: [GlobalBind] -> Maybe Term
        go [] = Nothing
        go ((v, _, t):xs)
            | v == var  = Just t
            | otherwise = go xs

-- | The instance of @HasGlobal@ for an environment containing global bindings.
instance HasGlobals Env where
    insertIntoGlobals pair env = env { globals = insertIntoGlobals pair $ globals env}
    getGlobalTerm var env = getGlobalTerm var $ globals env

