{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
The "TyArith.Repl" module defines the Read-Eval-Print-Loop
used by this implementation of the Typed Arithmetic language.
-}

module TyArith.Repl
    ( -- * The REPL
      repl
    ) where

import qualified Data.Text as T

import TyArith.Syntax ( Term )
import TyArith.Parser ( parseTerm )
import TyArith.Eval ( eval, step )
import TyArith.Pretty
    ( evalArrow, stepArrow, lastStepArrow, text )
import TyArith.Check ( (<?>), typeof, TypeErr )
import TyArith.Types ( Typ )

import Data.Text.Prettyprint.Doc ( (<+>), indent, pretty )
import Data.List as List ( isPrefixOf )
import Data.Void (Void)

import Text.Megaparsec ( runParser,  errorBundlePretty, ParseErrorBundle )

import Control.Monad.Trans.Class ( lift )

import System.Console.Haskeline

-- | The Read-Eval-Print-Loop of the TyArith language.
repl :: IO ()
repl = runInputT defaultSettings loop

-- | The main loop of the TyArith REPL.
loop :: InputT IO ()
loop = do
    line <- getInputLine "TyArith> "
    case line of
        Nothing                         -> outputStrLn "Leaving TyArith REPL."
        Just input -> case input of
            _ | ":q" `isPrefixOf` input -> outputStrLn "Leaving TyArith REPL."
            _ | ":a" `isPrefixOf` input -> lift (allStCmd $ removeCmd $ T.pack input) >> loop
            _ | ":p" `isPrefixOf` input -> lift (parseCmd $ removeCmd $ T.pack input) >> loop
            _ | ":s" `isPrefixOf` input -> lift (stepCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":t" `isPrefixOf` input -> lift (typeCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":e" `isPrefixOf` input -> lift (evalCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":"  `isPrefixOf` input -> outputStrLn "Command could not be parsed." >> loop
            _                           -> lift (evalCmd  $             T.pack input) >> loop

-- | Removes the REPL command from the input line.
removeCmd :: T.Text -> T.Text
removeCmd = T.dropWhile (== ' ') . T.dropWhile (/= ' ')

-- | Parses a text into a term.
parseLine :: T.Text -> Either (ParseErrorBundle T.Text Void) Term
parseLine = runParser parseTerm "tyarith"

-- | The command for parsing and printing the result.
parseCmd :: T.Text -> IO ()
parseCmd = parseExec pure print

-- | THe command for showing all the steps in the evaluation of a term.
allStCmd :: T.Text -> IO ()
allStCmd = parseExec (id <?>) allStepsPrint
  where
    -- | AllSteps-command helper.
    allStepsPrint :: Term -> IO ()
    allStepsPrint term = do    
        -- The first line is indented because the following ones have arrows.
        print . indent 4 $ pretty term
        recPrintSteps term
    -- | Recursive helper function to print all steps of the evaluation.
    recPrintSteps :: Term -> IO ()
    recPrintSteps t = case step t of
        Nothing -> print lastStepArrow
        Just t' -> do
            print $ stepArrow <+> pretty t'
            recPrintSteps t'

-- | The command for stepping an expression into another and pretty-printing its result.
stepCmd :: T.Text -> IO ()
stepCmd = parseExec (step <?>) stepPrint
  where
    -- | Helper printer function.
    stepPrint :: Maybe Term -> IO ()
    stepPrint term = print $ case term of
        Nothing -> lastStepArrow  -- The expression is either a value or stuck.
        Just t' -> stepArrow <+> pretty t'

-- | The command for fully evaluating an expression and pretty-printing its result.
evalCmd :: T.Text -> IO ()
evalCmd = parseExec (eval <?>) evalPrint
  where
    -- | Helper printer function.
    evalPrint :: Term -> IO ()
    evalPrint term = print $ evalArrow <+> pretty term

-- | The command for calculating the static type of an expression.
typeCmd :: T.Text -> IO ()
typeCmd = parseExec typeof typePrint
  where
    -- | Helper printer function.
    typePrint :: Typ -> IO ()
    typePrint typ = print $ text " <expr> :" <+> pretty typ

{- |
The @parseExec@ function is used to parse a 'T.Text' into a 'Term' 
and then either print the parsing error or execute a command on the
'Term' obtained by the parsing step.

The command returns a 'TypeErr' or a value: the printer function is then
used to print the type error or pretty-print the resulting value.
-}
parseExec :: (Term -> Either TypeErr a)  -- ^ The command to be executed on the parsed 'Term'.
          -> (a -> IO ())                -- ^ Printer function.
          -> T.Text                      -- ^ The initial text to be parsed.
          -> IO ()
parseExec cmd printer txt = case parseLine txt of
    Left parseErr -> putStr $ errorBundlePretty parseErr
    Right expr    -> case cmd expr of
        Left typeErr -> print $ pretty typeErr
        Right result -> printer result 