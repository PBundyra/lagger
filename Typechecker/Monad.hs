module Typechecker.Monad where

import Control.Monad.Except
import Control.Monad.State
import Prelude
import Typechecker.Environment
import Typechecker.Exceptions


type TypecheckerM = TypecheckerM' ()
type TypecheckerM' a = StateT Env (Except TypecheckingException) a
type TypegetterM = TypecheckerM' RawType

class Typechecker a where
  checkType :: a -> TypecheckerM
  checkType _ = error "Method not implemented"

  getType :: a -> TypegetterM
  getType _ = error "Method not implemented"
