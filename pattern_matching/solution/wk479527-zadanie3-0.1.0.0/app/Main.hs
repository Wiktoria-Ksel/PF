module Main where

import Context
import Control.Monad.State.Strict (runState)
import Parser
import Pretty (showWithFocus)
import ReducerTrace (reduceTrace)
import SnocList (toList)
import Syntax
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Trace
import Validate

main :: IO ()
main = do
  args <- getArgs
  input <- case args of
    ["--help"] -> do
      putStrLn "Usage: zadanie3 [--help] [file]"
      putStrLn " --help - display this message"
      putStrLn " file   - file with program to reduce"
      exitSuccess
    [] ->
      getContents
    [file] ->
      readFile file
    _ ->
      error "Usage: zadanie3 [--help] [file]"

  putStr input
  putStrLn (replicate 60 '-')

  let prog = fromHsString input

  case validateProg prog of
    Just err ->
      error err
    Nothing -> do
      let dMap = buildDefMap prog
          initialState =
            ReduceState
              { stDefs = dMap,
                stCounter = 0,
                stFuel = 30,
                stHistory = mempty
              }
          (_, finalState) =
            runState (reduceTrace (Var "main")) initialState

      mapM_
        putStrLn
        [ showWithFocus (stepPath step) (stepExpr step)
          | step <- take 30 (toList (stHistory finalState))
        ]
