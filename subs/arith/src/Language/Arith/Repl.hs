{-# LANGUAGE OverloadedStrings #-}

{- |
The "Language.Arith.Repl" module defines the Read-Eval-Print-Loop
used by this implementation of the Arith language.
-}

module Language.Arith.Repl
    ( -- * REPL
      repl
    ) where

import qualified Data.Text as T

import Language.Arith.Syntax ( Term(..) )
import Language.Arith.Parser ( parseTerm )
import Language.Arith.Eval ( step, eval )
import Language.Arith.Pretty ( evalArrow, stepArrow, lastStepArrow ) 

import Data.Text.Prettyprint.Doc

import Text.Megaparsec ( parse, ParseErrorBundle, errorBundlePretty )
import Data.Void ( Void )
import Data.List as List ( isPrefixOf )

import System.Console.Haskeline
import Control.Monad.Trans.Class ( lift )


-- | The Read-Eval-Print-Loop of the Arith language.
repl :: IO ()
repl = runInputT defaultSettings loop

-- | The main loop of the Arith REPL.
loop :: InputT IO ()
loop = do
    line <- getInputLine "Arith> "
    case line of
        Nothing                         -> outputStrLn "Leaving Arith REPL."
        Just input -> case input of
            _ | ":q" `isPrefixOf` input -> outputStrLn "Leaving Arith REPL."
            _ | ":a" `isPrefixOf` input -> lift (allStCmd $ removeCmd $ T.pack input) >> loop
            _ | ":p" `isPrefixOf` input -> lift (parseCmd $ removeCmd $ T.pack input) >> loop
            _ | ":s" `isPrefixOf` input -> lift (stepCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":e" `isPrefixOf` input -> lift (evalCmd  $ removeCmd $ T.pack input) >> loop
            _ | ":"  `isPrefixOf` input -> outputStrLn "Command could not be parsed." >> loop
            _                           -> lift (evalCmd  $             T.pack input) >> loop

-- | Removes the REPL command from the input line.
removeCmd :: T.Text -> T.Text
removeCmd = T.dropWhile (== ' ') . T.dropWhile (/= ' ')

-- | Parses a text into a term.
parseLine :: T.Text -> Either (ParseErrorBundle T.Text Void) Term
parseLine = parse parseTerm "arith"

-- | The command for parsing and printing the result.
parseCmd :: T.Text -> IO ()
parseCmd = parseExec print

-- | THe command for showing all the steps in the evaluation of a term.
allStCmd :: T.Text -> IO ()
allStCmd = parseExec allStepsPrint
  where
    -- | Helper command for 'allStCmd'.j
    allStepsPrint :: Term -> IO ()
    allStepsPrint term = do
        -- The first line is indented because the following ones have arrows.j
        print $ indent 4 $ pretty term
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
stepCmd = parseExec stepPrint
  where
    -- | Helper command for 'stepCmd'.
    stepPrint :: Term -> IO ()
    stepPrint t = print $ case step t of
        Nothing -> lastStepArrow                -- The expression is either a value or stuck.
        Just t' -> stepArrow <+> pretty t'

-- | The command for fully evaluating an expression and pretty-printing its result.
evalCmd :: T.Text -> IO ()
evalCmd = parseExec evalPrint
  where
    -- | Helper command for 'evalCmd'.
    evalPrint :: Term -> IO ()
    evalPrint term = print $ evalArrow <+> pretty (eval term)

{- |
The @parseExec@ function is used to parse a 'T.Text' into a 'Term' 
and then either print the parsing error or execute a command on the
'Term' obtained by the parsing step. 
-}
parseExec :: (Term -> IO ())
          -> T.Text
          -> IO ()
parseExec cmd txt = case parseLine txt of
    Left parseErr -> putStr $ errorBundlePretty parseErr
    Right term    -> cmd term