{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Arith.Repl
    ( repl
    ) where

import qualified Data.Text as T

import Arith.Syntax
import Arith.Parser
import Arith.Eval

import Text.Megaparsec ( parse )

import Data.Char ( isSpace )
import Data.List as List
import System.Console.Haskeline
import Control.Monad.Trans.Class ( lift ) 
import Control.Monad.IO.Class ( liftIO )

-- | The Read-Eval-Print-Loop of the Arith language.
repl :: IO ()
repl = runInputT defaultSettings loop

-- | The main loop of the Arith REPL.
loop :: InputT IO ()
loop = do
    line <- getInputLine "Arith> "
    case line of
        Nothing         -> outputStrLn "Leaving Arith REPL."
        -- Just (':' : cmd) -> lift (runCmd $ T.pack cmd) >> loop
        Just expr       -> lift (evalCmd $ T.pack expr) >> loop

-- Everything that follows this comment must be rethinked lol

commandList :: [(T.Text, T.Text -> IO ())]
commandList = [ ("quit", quitCmd)
              , ("eval", evalCmd)
              , ("step", stepCmd)
              , ("parse", parseCmd)]

runCmd :: T.Text -> IO ()
runCmd = dispatchCommand commandList
              
quitCmd :: T.Text -> IO ()
quitCmd _ = pure ()

parseCmd :: T.Text -> IO ()
parseCmd term = print $ parse parseTerm "<stdin>" term

stepCmd :: T.Text -> IO ()
stepCmd term = case parse parseTerm "<stdin>" term of
    Left err -> print err
    Right expr -> print $ eval expr

evalCmd :: T.Text -> IO ()
evalCmd term = case parse parseTerm "<stdin>" term of
    Left err -> print err
    Right expr -> print $ eval expr


