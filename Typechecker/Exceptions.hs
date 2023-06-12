{-# LANGUAGE FlexibleInstances #-}
module Typechecker.Exceptions where

import Common.Utils
import Lagger.Abs
import Typechecker.Environment


type TypecheckingException = TypecheckingException' BNFC'Position

data TypecheckingException' a =
    UndefinedReference a Ident
  | PredefinedFunction a Ident
  | VarCantBeFunc a Ident
  | DuplicateArgs a [Ident]
  | BreakOrContOutsideLoop a
  | NoMainFunc
  | NoReturn a
  | InvalidReference a
  | InvalidNumOfArgs a Int Int
  | TypeMismatch a (RawType, RawType)

instance Show TypecheckingException where
  show (UndefinedReference pos ident) =
    "Error: Undefined reference at " ++ showPos pos ++ ": " ++ show ident
  show (PredefinedFunction pos ident) =
    "Error: Predefined function used as an identifier at " ++ showPos pos ++ ": " ++ show ident
  show (VarCantBeFunc pos ident) =
    "Error: Variable cannot be a function at " ++ showPos pos ++ ": " ++ show ident
  show (DuplicateArgs pos idents) =
    "Error: Duplicate function arguments at " ++ showPos pos ++ ": " ++ show idents
  show (BreakOrContOutsideLoop pos) =
    "Error: Break or continue statement outside of a loop at " ++ showPos pos
  show NoMainFunc =
    "Error: No main function found"
  show (NoReturn pos) =
    "Error: No return statement found in function at " ++ showPos pos
  show (InvalidReference pos) =
    "Error: Invalid reference at " ++ showPos pos
  show (InvalidNumOfArgs pos expected actual) =
    "Error: Invalid number of arguments at " ++ showPos pos ++ ". Expected " ++ show expected ++ ", but got " ++ show actual
  show (TypeMismatch pos (expected, actual)) =
    "Error: Type mismatch at " ++ showPos pos ++ ". Expected " ++ show expected ++ ", but got " ++ show actual
