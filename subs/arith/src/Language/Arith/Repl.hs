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
import Language.Arith.Parser ( parseTerm, Parser, symbol )
import Language.Arith.Eval ( step, eval )
import Language.Arith.Pretty ( evalArrow, stepArrow, lastStepArrow ) 

import Data.Text.Prettyprint.Doc

import Text.Megaparsec hiding ( (<?>) )
import Data.Void ( Void )
import Data.List as List ( isPrefixOf )

import System.Console.Haskeline
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Trans.Class ( lift )


-- | The Read-Eval-Print-Loop of the Arith language.
repl :: IO ()
repl = runInputT defaultSettings loop

-- | The main loop of the Arith REPL.
loop :: InputT IO ()
loop = do
    line <- getInputLine "Arith> "
    case line of
        Nothing -> outputStrLn "Leaving Arith REPL."
        Just input -> dispatchCommand $ T.pack input

-- | A REPL Command.
data Command
    = QuitCmd           -- ^ Quitting the REPL.
    | HelpCmd           -- ^ Hasking for help.
    | WrongCmd          -- ^ Unknown command.
    | ParseCmd    Term  -- ^ Parsing.
    | EvalCmd     Term  -- ^ Fully evaluating.
    | StepCmd     Term  -- ^ Stepping an expression.
    | AllStepsCmd Term  -- ^ Fully evaluates and prints all the steps.
    
{- | 
Takes the contents of the REPL line, parses it 
and if it is a valid command it executes it. 
-}
dispatchCommand :: T.Text -> InputT IO ()
dispatchCommand txt = case runParser (parseCommand <* eof) "" txt of
    Left parseErr -> liftIO (putStr $ errorBundlePretty parseErr) *> loop
    Right cmd -> execCmd cmd
  
-- | Parses a REPL 'Command'.
parseCommand :: Parser Command
parseCommand = choice
    [ parseEvalCmd
    , parseParseCmd
    , parseStepCmd
    , parseAllStepsCmd
    , parseQuitCmd
    , parseHelpCmd
    , parseWrongCmd 
    ]

-- | Parses an 'EvalCmd'.
parseEvalCmd :: Parser Command
parseEvalCmd = do
    _ <- symbol ":eval" 
      <|> symbol ":e" 
      <|> (symbol "" <* notFollowedBy ":")
    EvalCmd <$> parseTerm

-- | Parses a 'ParseCmd'.
parseParseCmd :: Parser Command
parseParseCmd = do
    _ <- symbol ":parse"
      <|> symbol ":p"
    ParseCmd <$> parseTerm

-- | Parses a 'QuitCmd'.
parseQuitCmd :: Parser Command
parseQuitCmd = do
    _ <- symbol ":quit" 
      <|> symbol ":q" 
    pure QuitCmd

-- | Parses a 'StepCmd'.
parseStepCmd :: Parser Command
parseStepCmd = do
    _ <- symbol ":step"
      <|> symbol ":s"
    StepCmd <$> parseTerm

-- | Parses an 'AllStepsCmd'.
parseAllStepsCmd :: Parser Command
parseAllStepsCmd = do
    _ <- symbol ":allSteps"
      <|> symbol ":a"
    AllStepsCmd <$> parseTerm

-- | Parses a 'HelpCmd'.
parseHelpCmd :: Parser Command
parseHelpCmd = do
    _ <- symbol ":help"
      <|> symbol ":h"
    pure HelpCmd

-- | Parses an unknown command and returns an error.
parseWrongCmd :: Parser Command
parseWrongCmd = do
    _ <- symbol ":"
    cmd <- many (satisfy (/= ' '))
    fail $ "The command \":" <> cmd <> "\" is not a valid command. \
          \\nTo print a list of all the commands, use the command ':help' or ':h'."


{- |
'execCmd' takes a REPL 'Command' and it executes it. 
-}
execCmd :: Command -> InputT IO ()
-- Helper command.
execCmd HelpCmd = outputStrLn "List of option for the Arith REPL:" *> printHelpList *> loop
-- Command to quit the REPL.
execCmd QuitCmd = outputStrLn "Leaving Arith REPL."
-- Command to evaluate an expression using the multistep relation.
execCmd (EvalCmd term) = liftIO (evalPrint term) *> loop
  where
    -- | Helper command for 'evalCmd'.
    evalPrint :: Term -> IO ()
    evalPrint term = print $ evalArrow <+> pretty (eval term)
-- Command to parse an expression and print the resulting AST.
execCmd (ParseCmd term) = liftIO (print term) *> loop
-- Command to execute a single step of evaluation.
execCmd (StepCmd term) = liftIO (stepPrint term) *> loop
  where
    -- | Helper print function.
    stepPrint :: Term -> IO ()
    stepPrint t = case step t of
        Just t' -> print $ stepArrow <+> pretty t'        
        Nothing -> print lastStepArrow
-- Command to fully evaluate an expression and print all the steps.
execCmd (AllStepsCmd term) = liftIO (allStepsPrint term) *> loop
  where
    -- | Prints the complete result of the evaluation.
    allStepsPrint :: Term -> IO ()
    allStepsPrint term = do
        -- The first line is indented because the following ones have arrows.
        print $ indent 4 $ pretty term
        recPrintSteps term
    -- | Recursive helper function to print all steps of the evaluation.
    recPrintSteps :: Term -> IO ()
    recPrintSteps t = case step t of
        Nothing -> print lastStepArrow
        Just t' -> do
            print $ stepArrow <+> pretty t'
            recPrintSteps t'

printHelpList :: InputT IO ()
printHelpList = outputStrLn `mapM_` helpList
  where
    helpList :: [String]
    helpList =
        [ ":parse     / :p    Command to parse the given text and print the result of the parsing."
        , ":step      / :s    Command to step an expression into another expression."
        , ":allSteps  / :a    Command to print all the steps of the evaluation of a term."
        , ":eval      / :e    Command to print the final result of the evaluation of a term. \
            \Can be used without writing \":eval\" or \":e\"."
        , ":help      / :h    Command to print the list of commands."
        , ":quit      / :q    Command to quit the REPL."
        ]