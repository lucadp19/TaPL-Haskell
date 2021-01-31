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

import Data.Text.Prettyprint.Doc ( (<+>), indent, Doc, pretty )
import Data.List as List ( isPrefixOf )

import Control.Monad.Reader
import Control.Monad.Except ( runExceptT )

import Text.Megaparsec ( runParserT,  errorBundlePretty )

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
parseCmd text = do
    term <- runParserT parseTerm "simplytyp" text
    liftIO $ print term

-- | THe command for showing all the steps in the evaluation of a term.
allStCmd :: T.Text -> Eval ()
allStCmd term = runParserT parseTerm "simplytyp" term >>= \case
    Left err   -> liftIO . putStr . errorBundlePretty $ err
    Right expr -> do
        firstExpr <- prettyTerm expr
        -- The first line is indented because the following ones have arrows.
        liftIO . print . indent 4 $ firstExpr 
        printAllSteps expr
  where
    -- | Recursive helper function to print all steps of the evaluation.
    printAllSteps :: Term -> Eval ()
    printAllSteps t = case step t of
        Nothing -> liftIO $ print lastStepArrow
        Just t' -> do
            toPrint <- (stepArrow <+>) <$> prettyTerm t'
            liftIO $ print toPrint
            printAllSteps t'

-- | The command for stepping an expression into another and pretty-printing its result.
stepCmd :: T.Text -> Eval ()
stepCmd term = runParserT parseTerm "simplytyp" term >>= \case
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right expr -> do
        toPrint <- prettyStep $ step expr
        liftIO $ print toPrint
  where
    prettyStep :: Maybe Term -> Eval (Doc ann)
    prettyStep = \case
        Just t' -> (stepArrow <+>) <$> prettyTerm t'
        Nothing -> pure lastStepArrow

-- | The command to add a new global binding into the interpreter.
letCmd :: T.Text -> InputT Eval ()
letCmd term = lift (runParserT parseLet "simplytyp" term) >>= \case
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right (name, expr) -> lift $ runExceptT (typeof expr) >>= \case 
        Left tyErr -> liftIO $ print $ pretty tyErr
        Right ty -> let runLoop = runInputT defaultSettings loop in
            local (insertIntoGlobals (name, ty, expr)) runLoop

-- | The command for fully evaluating an expression and pretty-printing its result.
evalCmd :: T.Text -> Eval ()
evalCmd term = runParserT parseTerm "simplytyp" term >>= \case
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right expr -> runExceptT (eval <?> expr) >>= printErrOrEval
  where
    -- | Prints the type error or the evaluated expression.
    printErrOrEval :: Either TypeErr Term -> Eval ()
    printErrOrEval (Left typeErr) = liftIO $ print $ pretty typeErr
    printErrOrEval (Right evalExpr) = do
        toPrint <- (evalArrow <+>) <$> prettyTerm evalExpr
        liftIO $ print toPrint

-- | The command for calculating the type of an expression.
typeCmd :: T.Text -> Eval ()
typeCmd term = runParserT parseTerm "simplytyp" term >>= \case
    Left err -> liftIO . putStr . errorBundlePretty $ err
    Right expr -> runExceptT (typeof expr) >>= 
        liftIO . print . prettyErrOrType
  where
    -- | Prettyfies the type error or the expression's type.
    prettyErrOrType :: Either TypeErr Typ -> Doc ann
    prettyErrOrType (Left typeErr) = pretty typeErr
    prettyErrOrType (Right typ) = text " <expr> :" <+> pretty typ