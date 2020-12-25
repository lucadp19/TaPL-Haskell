{-# LANGUAGE OverloadedStrings #-}

module Arith.Repl
    ( repl
    , arith
    ) where

import qualified Data.Text as T

import Arith.Syntax
import Arith.Parser
import Arith.Eval
import Arith.Pretty 

import Data.Text.Prettyprint.Doc

import Text.Megaparsec ( parse, ParseErrorBundle, errorBundlePretty )
import Data.Void
import Data.Maybe ( fromMaybe )

import Data.Char ( isSpace )
import Data.List as List
import System.Console.Haskeline
import Control.Monad.Trans.Class ( lift ) 
import Control.Monad.IO.Class ( liftIO )

-- | The Read-Eval-Print-Loop of the Arith language.
repl :: IO ()
repl = runInputT defaultSettings loop

-- | The Read-Eval-Print-Loop of the Arith language.
arith :: IO ()
arith = repl

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
parseCmd = print . parseLine

-- | THe command for showing all the steps in the evaluation of a term.
allStCmd :: T.Text -> IO ()
allStCmd term = case parseLine term of
    Left err   -> putStr $ errorBundlePretty err
    Right expr -> do
        print $ indent 4 $ pretty expr -- The first line is indented because the following ones have arrows.
        printAllSteps expr

    where
        printAllSteps :: Term -> IO ()
        printAllSteps t = case step t of
            Nothing -> print lastStepArrow
            Just t' -> do
                print $ stepArrow <+> pretty t'
                printAllSteps t'

-- | The command for stepping an expression into another and pretty-printing its result.
stepCmd :: T.Text -> IO ()
stepCmd term = case parseLine term of
    Left err   -> putStr $ errorBundlePretty err
    Right t -> print $ case step t of
        Nothing -> lastStepArrow                -- The expression is either a value or stuck.
        Just t' -> stepArrow <+> pretty t'

-- | The command for fully evaluating an expression and pretty-printing its result.
evalCmd :: T.Text -> IO ()
evalCmd term = case parse parseTerm "<stdin>" term of
    Left err   -> putStr $ errorBundlePretty err
    Right expr -> print $ evalArrow <+> pretty (eval expr)


