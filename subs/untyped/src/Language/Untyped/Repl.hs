{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
The "Language.Untyped.Repl" module defines the Read-Eval-Print-Loop
used by this implementation of the Untyped Lambda Calculus.
-}

module Language.Untyped.Repl
    ( repl
    ) where

import qualified Data.Text as T

import Language.Untyped.Syntax ( Term )
import Language.Untyped.Parser ( parseTerm, parseLet )
import Language.Untyped.Eval ( eval, step )
import Language.Untyped.Environment ( emptyEnv, insertIntoGlobals )
import Language.Untyped.Pretty
    ( evalArrow, stepArrow, lastStepArrow, prettyTerm ) 
import Language.Untyped.Monad ( Eval(runEval) )

import Data.Text.Prettyprint.Doc ( (<+>), indent, Doc )
import Data.List as List ( isPrefixOf )

import Control.Monad.Reader

import Text.Megaparsec ( runParserT,  errorBundlePretty )

import System.Console.Haskeline

-- | The Read-Eval-Print-Loop of the Untyped language.
repl :: IO ()
repl = run (runInputT defaultSettings loop)
  where 
    run :: Eval a -> IO a
    run t = runReaderT (runEval t) emptyEnv

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
parseCmd = parseExec $ liftIO . print

-- | THe command for showing all the steps in the evaluation of a term.
allStCmd :: T.Text -> Eval ()
allStCmd = parseExec allStepsPrint
  where
    allStepsPrint :: Term -> Eval ()
    allStepsPrint term = do
        firstExpr <- prettyTerm term
        -- The first line is indented because the following ones have arrows.
        liftIO $ print $ indent 4 firstExpr
        recPrintSteps term
    -- | Recursive helper function to print all steps of the evaluation.
    recPrintSteps :: Term -> Eval ()
    recPrintSteps t = case step t of
        Nothing -> liftIO $ print lastStepArrow
        Just t' -> do
            toPrint <- (stepArrow <+>) <$> prettyTerm t'
            liftIO $ print toPrint
            recPrintSteps t'

-- | The command for stepping an expression into another and pretty-printing its result.
stepCmd :: T.Text -> Eval ()
stepCmd = parseExec stepPrint
  where
    stepPrint term = do
        toPrint <- prettyStep $ step term
        liftIO $ print toPrint
    prettyStep :: Maybe Term -> Eval (Doc ann)
    prettyStep = \case
        Just t' -> (stepArrow <+> ) <$> prettyTerm t'
        Nothing -> pure lastStepArrow

-- | The command to add a new global binding into the interpreter.
letCmd :: T.Text -> InputT Eval ()
letCmd term = lift (runParserT parseLet "untyped" term) >>= \case
    Left err -> liftIO $ putStr $ errorBundlePretty err
    Right (name, expr) -> lift $
        let runLoop = runInputT defaultSettings loop in
            local (insertIntoGlobals (name, expr)) runLoop

-- | The command for fully evaluating an expression and pretty-printing its result.
evalCmd :: T.Text -> Eval ()
evalCmd = parseExec evalPrint
  where
    evalPrint :: Term -> Eval ()
    evalPrint term = do
        toPrint <- (evalArrow <+> ) <$> prettyTerm (eval term)
        liftIO $ print toPrint

{- |
The @parseExec@ function is used to parse a 'T.Text' into a 'Term' 
and then either print the parsing error or execute a command on the
'Term' obtained by the parsing step. 

The given command returns a type error or a result, which in turn is pretty
printed with the help of a printer function.
-}
parseExec :: (Term -> Eval ())   -- ^ The command to be executed on the parsed 'Term'.
          -> T.Text              -- ^ The initial text to be parsed.
          -> Eval ()
parseExec cmd txt = runParserT parseTerm "untyped" txt >>= \case
    Left parseErr -> liftIO $ putStr $ errorBundlePretty parseErr
    Right term    -> cmd term