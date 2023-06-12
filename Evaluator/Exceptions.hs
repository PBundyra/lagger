{-# LANGUAGE FlexibleInstances #-}
module Evaluator.Exceptions where

import Common.Utils
import Lagger.Abs


type RuntimeException = RuntimeException' BNFC'Position

data RuntimeException' a =
    ArithmeticException a
  | UserRuntimeException a String

instance Show RuntimeException where
  show (ArithmeticException pos) =
    "RuntimeException: Arithmetic exception at: " ++ showPos pos
  show (UserRuntimeException pos msg) =
    "RuntimeException: " ++ msg ++ " at: " ++ showPos pos
