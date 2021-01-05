{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Untyped.Repl
    ( repl
    , untyped
    ) where

import qualified Data.Text as T

import Untyped.Syntax ( Term )
import Untyped.Parser ( parseTerm, parseLet )
import Untyped.Eval ( eval, step )
import Untyped.Environment ( Env, emptyEnv, insertIntoGlobals )
import Untyped.Pretty 
import Untyped.Monad

import Data.Text.Prettyprint.Doc

import Text.Megaparsec ( runParserT, ParseErrorBundle, errorBundlePretty )
import Data.Void ( Void )
import Data.Maybe ( fromMaybe )

import Data.Char ( isSpace )
import Data.List as List ( isPrefixOf )
import System.Console.Haskeline
import Control.Monad.Reader
import Control.Monad.Trans.Class ( lift ) 
import Control.Monad.IO.Class ( liftIO )

-- | The Read-Eval-Print-Loop of the Untyped language.
repl :: IO ()
repl = run (runInputT defaultSettings loop)
  where 
    run :: Eval a -> IO a
    run t = runReaderT (runEval t) emptyEnv

-- | The Read-Eval-Print-Loop of the Untyped language.
untyped :: IO ()
untyped = repl

-- | The main loop of the Untyped REPL.
loop :: InputT Eval ()
loop = do
    line <- getInputLine "Untyped> "
    case line of
        Nothing                         -> outputStrLn "Leaving Untyped REPL."
        Just input -> case input of
            _ | ":q" `isPrefixOf` input -> outputStrLn "Leaving Untyped REPL."
            _ | ":a" `isPrefixOf` input -> lift (allStCmd $ removeCmd $ T.pack input) >> loop
            _ | ":p" `isPrefixOf` input -> lift (parseCmd $ removeCmd $ T.pack input) >> loop
            _ | ":s" `isPrefixOf` input -> lift (stepCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":e" `isPrefixOf` input -> lift (evalCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":l" `isPrefixOf` input ->       letCmd   $ removeCmd $ T.pack input
            _ | ":"  `isPrefixOf` input -> outputStrLn "Command could not be parsed." >> loop
            _                           -> lift (evalCmd  $             T.pack input) >> loop

-- | Removes the REPL command from the input line.
removeCmd :: T.Text -> T.Text
removeCmd = T.dropWhile (== ' ') . T.dropWhile (/= ' ')

-- | The command for parsing and printing the result.
parseCmd :: T.Text -> Eval ()
parseCmd text = do
    term <- runParserT parseTerm "untyped" text
    liftIO $ print term

-- | THe command for showing all the steps in the evaluation of a term.
allStCmd :: T.Text -> Eval ()
allStCmd term = runParserT parseTerm "untyped" term >>= \case
    Left err   -> liftIO . putStr . errorBundlePretty $ err
    Right expr -> do
        (liftIO :: IO () -> Eval ()) 
            . print 
            . indent 4 
            <$> prettyEval expr -- The first line is indented because the following ones have arrows.
        printAllSteps expr
  where
    -- | Recursive helper function to print all steps of the evaluation.
    printAllSteps :: Term -> Eval ()
    printAllSteps t = case step t of
        Nothing -> liftIO $ print lastStepArrow
        Just t' -> do
            toPrint <- (stepArrow <+>) <$> prettyEval t'
            liftIO $ print toPrint
            printAllSteps t'

-- | The command for stepping an expression into another and pretty-printing its result.
stepCmd :: T.Text -> Eval ()
stepCmd term = runParserT parseTerm "untyped" term >>= \case
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right expr -> do
        toPrint <- prettyStep $ step expr
        liftIO $ print toPrint
  where
    prettyStep :: Maybe Term -> Eval (Doc ann)
    prettyStep = \case
        Just t' -> (stepArrow <+> ) <$> prettyEval t'
        Nothing -> pure lastStepArrow

-- | The command to add a new global binding into the interpreter.
letCmd :: T.Text -> InputT Eval ()
letCmd term = lift (runParserT parseLet "untyped" term) >>= \case
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right p@(name, expr) -> lift $
        let runLoop = runInputT defaultSettings loop in
            local (insertIntoGlobals p) runLoop

-- | The command for fully evaluating an expression and pretty-printing its result.
evalCmd :: T.Text -> Eval ()
evalCmd term = runParserT parseTerm "untyped" term >>= \case
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right expr -> do
        toPrint <- (evalArrow <+> ) <$> prettyEval (eval expr)
        liftIO $ print toPrint