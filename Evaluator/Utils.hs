module Evaluator.Utils where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Evaluator.Exceptions
import Evaluator.Monad
import Evaluator.State
import Lagger.Abs


-- Methods related to predefined functions --
execPredefined :: BNFC'Position -> Ident -> [Value] -> EvaluatorM
execPredefined pos (Ident "error") [value] = do
  void $ throwError $ UserRuntimeException pos (show value)
  pure $ VInt 1

execPredefined _ (Ident "println") [value] = do
  liftIO $ putStrLn (show value)
  pure $ VInt 0


-- Methods related to types --
intToVInt :: Integer -> Value
intToVInt = VInt

vIntToInt :: Value -> Integer
vIntToInt (VInt val) = val

boolToVBool :: Bool -> Value
boolToVBool = VBool

vBoolToBool :: Value -> Bool
vBoolToBool (VBool val) = val

isTrue :: Value -> Bool
isTrue (VBool True) = True
isTrue _ = False


-- Misc methods --
getRetVal :: EvaluatorM
getRetVal = do
  (State env store flags) <- get
  let retVal = fromJust (fRet flags)
  put $ State env store emptyFlags
  pure retVal
