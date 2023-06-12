module Evaluator.Monad where

import Control.Monad.Except
import Control.Monad.State
import Evaluator.Exceptions
import Prelude
import qualified Evaluator.State as S


type EvaluatorM = EvaluatorM' S.Value
type EvaluatorM' a = StateT S.State (ExceptT RuntimeException IO) a
type EvaluatorStmt = EvaluatorM' ()

class Evaluator a where
  evalVal :: a -> EvaluatorM
  evalVal _ = error "Method not implemented"

  eval :: a -> EvaluatorStmt
  eval _ = error "Method not implemented"
