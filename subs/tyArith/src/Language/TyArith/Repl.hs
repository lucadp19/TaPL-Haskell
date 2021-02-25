{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{- |
The "Language.TyArith.Repl" module defines the Read-Eval-Print-Loop
used by this implementation of the Typed Arithmetic language.
-}

module Language.TyArith.Repl
    ( -- * The REPL
      repl
    ) where

import qualified Data.Text as T

import Language.TyArith.Syntax ( Term )
import Language.TyArith.Parser ( parseTerm, Parser, symbol )
import Language.TyArith.Eval ( eval, step )
import Language.TyArith.Pretty
    ( evalArrow, stepArrow, lastStepArrow, text )
import Language.TyArith.Check ( (<?>), typeof, TypeErr )
import Language.TyArith.Types ( Typ )

import Data.Text.Prettyprint.Doc ( (<+>), indent, pretty )
import Data.List as List ( isPrefixOf )
import Data.Void (Void)

import Text.Megaparsec hiding ( (<?>) )
import Control.Monad.IO.Class ( liftIO )
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
        Nothing -> outputStrLn "Leaving TyArith REPL."
        Just input -> dispatchCommand $ T.pack input

-- | A REPL Command.
data Command
    = QuitCmd           -- ^ Quitting the REPL.
    | HelpCmd           -- ^ Hasking for help.
    | WrongCmd          -- ^ Unknown command.
    | ParseCmd    Term  -- ^ Parsing.
    | TypeCmd     Term  -- ^ Type of an expression.
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
    , parseTypeCmd
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

-- | Parses a 'TypeCmd'.
parseTypeCmd :: Parser Command
parseTypeCmd = do
    _ <- symbol ":type"
      <|> symbol ":t"
    TypeCmd <$> parseTerm

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
    fail $ "The command \":" <> cmd <> "\" is not a valid command."

{- |
'execCmd' takes a REPL 'Command' and it executes it. 
-}
execCmd :: Command -> InputT IO ()
-- Helper command.
execCmd HelpCmd = outputStrLn "List of option for the TyArith REPL:" *> printHelpList *> loop
-- Command to quit the REPL.
execCmd QuitCmd = outputStrLn "Leaving TyArith REPL."
-- Command to evaluate an expression using the multistep relation.j
execCmd (EvalCmd term) = printTyErrOrExec (eval <?>) evalPrint term *> loop
  where
    -- | Helper command for 'evalCmd'.
    evalPrint :: Term -> IO ()
    evalPrint term = print $ evalArrow <+> pretty term
-- Command to parse an expression and print the resulting AST.
execCmd (ParseCmd term) = printTyErrOrExec pure print term *> loop
-- Command to execute a single step of evaluation.
execCmd (StepCmd term) = printTyErrOrExec (step <?>) stepPrint term *> loop
  where
    -- | Helper print function.
    stepPrint :: Maybe Term -> IO ()
    stepPrint = \case
        Just t' -> print $ stepArrow <+> pretty t'        
        Nothing -> print lastStepArrow
-- Command to fully evaluate an expression and print all the steps.
execCmd (AllStepsCmd term) = printTyErrOrExec (id <?>) allStepsPrint term *> loop
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
-- Command to compute the type of a term.
execCmd (TypeCmd term) = printTyErrOrExec typeof typePrint term *> loop
  where
    -- | Helper command for 'typeCmd'.
    typePrint :: Typ -> IO ()
    typePrint typ = print $ text " <expr> :" <+> pretty typ

printHelpList :: InputT IO ()
printHelpList = outputStrLn `mapM_` helpList
  where
    helpList :: [String]
    helpList =
        [ ":parse     / :p    Command to parse the given text and print the result of the parsing."
        , ":type      / :t    Command to print the type of a term (or a type error)."
        , ":step      / :s    Command to step an expression into another expression."
        , ":allSteps  / :a    Command to print all the steps of the evaluation of a term."
        , ":eval      / :e    Command to print the final result of the evaluation of a term. \
            \Can be used without writing \":eval\" or \":e\"."
        , ":help      / :h    Command to print the list of commands."
        , ":quit      / :q    Command to quit the REPL."
        ]

{- |
The 'printTyErrOrExec' function takes two function 
(a @cmd@ function and a @printer@ function) and a term. 

The @cmd@ function
must return either a 'TypeErr'or or a result: if it returns the former 
the 'printTyErrOrExec' function prints the type error; otherwise it prints
the result of the @cmd@ through the @printer@ function.
-}
printTyErrOrExec
    :: (Term -> Either TypeErr a)   -- ^ The command to be executed on the parsed 'Term'.
    -> (a -> IO ())                 -- ^ Printer function.
    -> Term                         -- ^ The initial text to be parsed.
    -> InputT IO ()
printTyErrOrExec cmd printer term = lift $ case cmd term of
    Left typeErr -> print $ pretty typeErr
    Right value  -> printer value

