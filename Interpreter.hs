module Interpreter where

import Evaluator.Evaluator (evalProgram)
import Lagger.Abs
import Lagger.Par
import System.Exit
import System.IO
import Typechecker.Typechecker (checkTypeProgram)

interpretFile :: FilePath -> IO ()
interpretFile file = readFile file >>= interpret

interpret :: String -> IO ()
interpret input =
  case parseProgram input of
    Left err -> printErrorAndExit err
    Right program -> do
      typecheckProgram program
      runProgram program

parseProgram :: String -> Either String Program
parseProgram input = pProgram $ myLexer input

typecheckProgram :: Program -> IO ()
typecheckProgram tree =
  case checkTypeProgram tree of
    Left err -> printErrorAndExit err
    Right _ -> pure ()

runProgram :: Program -> IO ()
runProgram program = do
  executionResult <- evalProgram program
  case executionResult of
    Left err -> printErrorAndExit err
    Right _ -> exitSuccess

printErrorAndExit :: (Show a) => a -> IO b
printErrorAndExit err = hPrint stderr err >> exitFailure
