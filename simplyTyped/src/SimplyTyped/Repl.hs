{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
The "SimplyTyped.Repl" module defines the Read-Eval-Print-Loop
used by this implementation of the Simply Typed Lambda Calculus.
-}

module SimplyTyped.Repl
    ( repl
    ) where

import qualified Data.Text as T

import SimplyTyped.Syntax ( Term )
import SimplyTyped.Parser ( parseTerm, parseLet )
import SimplyTyped.Eval ( eval, step )
import SimplyTyped.Env ( emptyEnv, insertIntoGlobals )
import SimplyTyped.Pretty
    ( evalArrow, stepArrow, lastStepArrow, prettyTerm, text ) 
import SimplyTyped.Monad ( Eval(runEval) )
import SimplyTyped.Check ( typeof, (<?>), TypeErr )
import SimplyTyped.Types ( Typ )

import Data.Text.Prettyprint.Doc ( (<+>), indent, pretty )
import Data.List as List ( isPrefixOf )
import Data.Void ( Void )

import Control.Monad.Reader
import Control.Monad.Except ( ExceptT, runExceptT )

import Text.Megaparsec ( runParserT, errorBundlePretty, ParseErrorBundle )

import System.Console.Haskeline

-- | The Read-Eval-Print-Loop of the SimplyTyped language.
repl :: IO ()
repl = run $ runInputT defaultSettings loop
  where 
    run :: Eval a -> IO a
    run t = runReaderT (runEval t) emptyEnv

-- | The main loop of the SimplyTyped REPL.
loop :: InputT Eval ()
loop = do
    line <- getInputLine "SimplyTyped> "
    case line of
        Nothing                         -> outputStrLn "Leaving SimplyTyped REPL."
        Just input -> case input of
            _ | ":q" `isPrefixOf` input -> outputStrLn "Leaving SimplyTyped REPL."
            _ | ":a" `isPrefixOf` input -> lift (allStCmd $ removeCmd $ T.pack input) >> loop
            _ | ":p" `isPrefixOf` input -> lift (parseCmd $ removeCmd $ T.pack input) >> loop
            _ | ":s" `isPrefixOf` input -> lift (stepCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":e" `isPrefixOf` input -> lift (evalCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":t" `isPrefixOf` input -> lift (typeCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":l" `isPrefixOf` input ->       letCmd   $ removeCmd $ T.pack input -- auto loops
            _ | ":"  `isPrefixOf` input -> outputStrLn "Command could not be parsed." >> loop
            _                           -> lift (evalCmd  $             T.pack input) >> loop

-- | Removes the REPL command from the input line.
removeCmd :: T.Text -> T.Text
removeCmd = T.dropWhile (== ' ') . T.dropWhile (/= ' ')

-- | The command for parsing and printing the result.
parseCmd :: T.Text -> Eval ()
parseCmd = parseExec pure (liftIO . print)

-- | THe command for showing all the steps in the evaluation of a term.
allStCmd :: T.Text -> Eval ()
allStCmd = parseExec (id <?>) allStepsPrint 
  where
    -- | Prints the complete result of the evaluation.
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
stepCmd = parseExec (step <?>) stepPrint
  where
    -- | Helper print function.
    stepPrint :: Maybe Term -> Eval ()
    stepPrint t = case t of
        Just t' -> do
            toPrint <- (stepArrow <+>) <$> prettyTerm t'
            liftIO $ print toPrint            
        Nothing -> liftIO $ print lastStepArrow

-- | The command to add a new global binding into the interpreter.
letCmd :: T.Text -> InputT Eval ()
letCmd term = lift (runParserT parseLet "stlc" term) >>= \case
    Left err -> printParseErr err >> loop
    Right (name, expr) -> lift (runExceptT $ typeof expr) >>= \case 
        Left tyErr -> printTypeErr tyErr >> loop
        Right ty -> mapInputT (local $ insertIntoGlobals (name, ty, expr)) loop
  where
    printParseErr :: ParseErrorBundle T.Text Void -> InputT Eval ()
    printParseErr = liftIO . putStr . errorBundlePretty
    printTypeErr :: TypeErr -> InputT Eval ()
    printTypeErr = liftIO . print . pretty

-- | The command for fully evaluating an expression and pretty-printing its result.
evalCmd :: T.Text -> Eval ()
evalCmd = parseExec (eval <?>) evalPrint
  where
    -- | Helper command for 'evalCmd'.
    evalPrint :: Term -> Eval ()
    evalPrint term = do
        toPrint <- (evalArrow <+>) <$> prettyTerm term
        liftIO $ print toPrint

-- | The command for calculating the type of an expression.
typeCmd :: T.Text -> Eval ()
typeCmd = parseExec typeof typePrint
  where
    -- | Helper command for 'typeCmd'.
    typePrint :: Typ -> Eval ()
    typePrint typ = liftIO $ print $ text " <expr> :" <+> pretty typ

{- |
The @parseExec@ function is used to parse a 'T.Text' into a 'Term' 
and then either print the parsing error or execute a command on the
'Term' obtained by the parsing step. 

The given command returns a type error or a result, which in turn is pretty
printed with the help of a printer function.
-}
parseExec :: (Term -> ExceptT TypeErr Eval a)   -- ^ The command to be executed on the parsed 'Term'.
          -> (a -> Eval ())                     -- ^ Printer function.
          -> T.Text                             -- ^ The initial text to be parsed.
          -> Eval ()
parseExec cmd printer txt = runParserT parseTerm "stlc" txt >>= \case
    Left parseErr -> liftIO $ putStr $ errorBundlePretty parseErr
    Right term    -> printTyErrOrExec term
  where
    printTyErrOrExec :: Term -> Eval ()
    printTyErrOrExec term = runExceptT (cmd term) >>= \case
        Left typeErr -> liftIO $ print $ pretty typeErr
        Right value  -> printer value