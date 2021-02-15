{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.FullUntyped.Repl
    ( repl
    , untyped
    ) where

import qualified Data.Text as T

import Language.FullUntyped.Syntax ( Term )
import Language.FullUntyped.Parser ( parseTerm, parseLet )
import Language.FullUntyped.Eval ( eval, step )
import Language.FullUntyped.Environment ( emptyEnv, insertIntoGlobals )
import Language.FullUntyped.Pretty
    ( evalArrow, stepArrow, lastStepArrow, prettyEval ) 
import Language.FullUntyped.Monad ( Eval(runEval) )

import Data.Text.Prettyprint.Doc ( (<+>), indent, Doc )
import Data.List as List ( isPrefixOf )

import Control.Monad.Reader

import Text.Megaparsec ( runParserT,  errorBundlePretty )

import System.Console.Haskeline

-- | The Read-Eval-Print-Loop of the FullUntyped language.
repl :: IO ()
repl = run $ runInputT defaultSettings loop
  where 
    run :: Eval a -> IO a
    run t = runReaderT (runEval t) emptyEnv

-- | The Read-Eval-Print-Loop of the FullUntyped language.
untyped :: IO ()
untyped = repl

-- | The main loop of the FullUntyped REPL.
loop :: InputT Eval ()
loop = do
    line <- getInputLine "FullUntyped> "
    case line of
        Nothing                         -> outputStrLn "Leaving FullUntyped REPL."
        Just input -> case input of
            _ | ":q" `isPrefixOf` input -> outputStrLn "Leaving FullUntyped REPL."
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
        firstExpr <- prettyEval expr
        -- The first line is indented because the following ones have arrows.
        liftIO . print . indent 4 $ firstExpr 
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
        Just t' -> (stepArrow <+>) <$> prettyEval t'
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
        toPrint <- (evalArrow <+>) <$> prettyEval (eval expr)
        liftIO $ print toPrint