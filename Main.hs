module Main where

import Interpreter
import System.Environment
import System.Exit

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> helpMessage
    [f] -> interpretFile f
    [] -> interactiveInterpreter
    _ -> helpMessage

helpMessage :: IO ()
helpMessage = do
  putStr
    "WelcomeXYZ to the Lagger programming language!\n\n\
    \Valid arguments:\n\n\
    \filename         Interpret from file\n\
    \[NO_ARGUMENTS]   Interpret in interactive mode from the stdin\n\
    \--help           Display the help message.\n"
  exitFailure

interactiveInterpreter :: IO ()
interactiveInterpreter = do
  getContents >>= interpret
