{-# LANGUAGE LambdaCase #-}
module Typechecker.Utils where

import Common.Utils
import Control.Monad.Except
import Control.Monad.State
import Data.List
import Prelude
import Lagger.Abs
import Typechecker.Environment
import Typechecker.Exceptions
import Typechecker.Monad


assertIsNotPredefined :: BNFC'Position -> Ident -> TypecheckerM
assertIsNotPredefined pos ident = do
  let isPredefinedFunc = isPredefined ident
  if isPredefinedFunc
  then throwError $ PredefinedFunction pos ident
  else pure ()

getArgIdent :: Arg -> Ident
getArgIdent (VarArg _ _ ident) = ident
getArgIdent (RefArg _ _ ident) = ident

assertArgsAreUnique :: BNFC'Position -> [Arg] -> TypecheckerM
assertArgsAreUnique pos args = do
  let idents = map getArgIdent args
  let uniqueIdents = nub idents
  if length idents == length uniqueIdents
  then pure ()
  else throwError $ DuplicateArgs pos idents

assertIdentIsNotFunc :: BNFC'Position -> Ident -> TypecheckerM
assertIdentIsNotFunc pos ident = do
  isFunc <- gets $ isFunc ident
  if isFunc
  then throwError $ VarCantBeFunc pos ident
  else pure ()

assertIsInsideLoop :: BNFC'Position -> TypecheckerM
assertIsInsideLoop pos = do
  isInsideLoop <- gets $ getInsideLoop
  if isInsideLoop
  then pure ()
  else throwError $ BreakOrContOutsideLoop pos

assertIsValidReference :: BNFC'Position -> Arg -> Expr -> TypecheckerM
assertIsValidReference _ (VarArg _ _ _) _ = pure()
assertIsValidReference _ (RefArg _ _ _) (EVar _ _) = pure()
assertIsValidReference pos (RefArg _ _ _) _ = throwError $ InvalidReference pos

assertIsCorrectNumOfArgs :: BNFC'Position -> Int -> [Expr] -> TypecheckerM
assertIsCorrectNumOfArgs pos numOfArgs exprs = do
  if numOfArgs == length exprs
  then pure ()
  else throwError $ InvalidNumOfArgs pos numOfArgs (length exprs)

assertHasMainFunc :: [TopDef] -> TypecheckerM
assertHasMainFunc topDefs = do
  let hasMainFunc = any (\case {(FnDef _ _ (Ident "main") _ _) -> True; _ -> False}) topDefs
  if hasMainFunc
  then pure ()
  else throwError $ NoMainFunc

hasReturn :: Block -> Bool
hasReturn (BBlock _ stmts) =
     any (\case { (Ret _ _) -> True; _ -> False }) stmts
  || any (\case { (IfCond _ _ block) -> hasReturn block; _ -> False }) stmts
  || any (\case { (CondElse _ _ ifBlock elseBlock) -> hasReturn ifBlock || hasReturn elseBlock; _ -> False }) stmts
  || any (\case { (While _ _ block) -> hasReturn block; _ -> False }) stmts

assertHasReturn :: BNFC'Position -> Block -> TypecheckerM
assertHasReturn pos block = do
  if hasReturn block
  then pure ()
  else throwError $ NoReturn pos

assertTypesAreEqual :: BNFC'Position -> RawType -> RawType -> TypecheckerM
assertTypesAreEqual position expectedType actualType =
  if expectedType == actualType
  then pure ()
  else throwError $ TypeMismatch position (expectedType, actualType)
