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

import Core.Parser( symbol )
import Core.Pretty ( evalArrow, stepArrow, lastStepArrow )

import Language.Untyped.Syntax ( Term )
import Language.Untyped.Parser ( parseTerm, parseLet )
import Language.Untyped.Eval ( eval, step )
import Language.Untyped.Environment ( emptyEnv, insertIntoGlobals, createGlobalBind )
import Language.Untyped.Pretty ( prettyTerm ) 
import Language.Untyped.Monad ( Eval, reduceEval )

import Data.Void ( Void )
import Data.Text.Prettyprint.Doc ( (<+>), indent, Doc )
import Data.List as List ( isPrefixOf )

import Control.Monad.Reader

import Text.Megaparsec hiding ( (<?>) )

import System.Console.Haskeline

-- | The Read-Eval-Print-Loop of the Untyped language.
repl :: IO ()
repl = reduceEval emptyEnv $ runInputT defaultSettings loop

-- | The main loop of the Untyped REPL.
loop :: InputT Eval ()
loop = do
    line <- getInputLine "Untyped> "
    case line of
        Nothing -> outputStrLn "Leaving Untyped REPL."
        Just input -> dispatchCommand $ T.pack input

{- |
The 'Command's that can be executed by the REPL.
-}
data Command
    = EvalCmd Term      -- ^ Evaluates a term.
    | ParseCmd Term     -- ^ Parses a term.
    | StepCmd Term      -- ^ Steps a term.
    | AllStepsCmd Term  -- ^ Fully-evaluates a term and prints all the steps.
    | TypeCmd Term      -- ^ Computes the type of a term.
    | GlobalLetCmd 
        (T.Text, Term)  -- ^ Adds a new global binding.
    | HelpCmd           -- ^ Prints the helping text.
    | QuitCmd           -- ^ Quits the REPL.

{- | 
Takes the contents of the REPL line, parses it 
and if it is a valid command it executes it. 
-}
dispatchCommand :: T.Text -> InputT Eval ()
dispatchCommand txt = lift (runParserT (parseCommand <* eof) "" txt) >>= \case
    Left parseErr -> lift (liftIO $ putStr $ errorBundlePretty parseErr) *> loop
    Right cmd -> execCmd cmd

type Parser = ParsecT Void T.Text Eval

-- | Parses a REPL 'Command'.
parseCommand :: Parser Command
parseCommand = choice
    [ parseEvalCmd
    , parseParseCmd
    , parseStepCmd
    , parseAllStepsCmd
    , parseTypeCmd
    , parseGlobalLetCmd
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

-- | Parses a 'HelpCmd'.
parseHelpCmd :: Parser Command
parseHelpCmd = do
    _ <- symbol ":help"
      <|> symbol ":p"
    pure HelpCmd

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

-- | Parses a 'AllStepsCmd'.
parseAllStepsCmd :: Parser Command
parseAllStepsCmd = do
    _ <- symbol ":allSteps"
      <|> symbol ":a"
    AllStepsCmd <$> parseTerm

-- | Parses a 'GlobalLetCmd'.
parseGlobalLetCmd :: Parser Command
parseGlobalLetCmd = do
    _ <- symbol ":let"
      <|> symbol ":l"
    GlobalLetCmd <$> parseLet

-- | Parses a 'TypeCmd'.
parseTypeCmd :: Parser Command
parseTypeCmd = do
    _ <- symbol ":type"
      <|> symbol ":t"
    TypeCmd <$> parseTerm

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
execCmd :: Command -> InputT Eval ()
-- Helper command.
execCmd HelpCmd = outputStrLn "List of option for the TyArith REPL:" *> printHelpList *> loop
-- Command to quit the REPL.
execCmd QuitCmd = outputStrLn "Leaving ExtSimple REPL."
-- Command to evaluate an expression using the multistep relation.j
execCmd (EvalCmd term) = lift (evalPrint $ eval term) *> loop
  where
    -- | Helper command for 'evalCmd'.
    evalPrint :: Term -> Eval ()
    evalPrint term = do
        toPrint <- (evalArrow <+>) <$> prettyTerm term
        liftIO $ print toPrint
-- Command to parse an expression and print the resulting AST.
execCmd (ParseCmd term) = liftIO (print term) *> loop
-- Command to execute a single step of evaluation.
execCmd (StepCmd term) = lift (stepPrint $ step term) *> loop
  where
    -- | Helper print function.
    stepPrint :: Maybe Term -> Eval ()
    stepPrint = \case
        Just t' -> do
            toPrint <- (stepArrow <+>) <$> prettyTerm t'
            liftIO $ print toPrint            
        Nothing -> liftIO $ print lastStepArrow
-- Command to fully evaluate an expression and print all the steps.
execCmd (AllStepsCmd term) = lift (allStepsPrint term) *> loop
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
-- Command to add a new global binding.
execCmd (GlobalLetCmd (var, term)) =
    mapInputT (local $ insertIntoGlobals $ createGlobalBind var term) loop

-- | Prints the help message.
printHelpList :: InputT Eval ()
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
        , ":let       / :l    Command to assign a new global name. Syntax is \n\t\t:let <var> = <term>"
        , ":help      / :h    Command to print the list of commands."
        , ":quit      / :q    Command to quit the REPL."
        ]