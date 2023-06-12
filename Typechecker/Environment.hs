{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Typechecker.Environment where

import Lagger.Abs
import Prelude
import qualified Data.Map as M


data Env = Env
  { types :: M.Map Ident RawType,
    funcs :: M.Map Ident (RawType, [Arg], Block),
    retT :: RawType,
    isInsideLoop :: Bool
  }

data RawType
    = RTInt
    | RTStr
    | RTBool
  deriving (Eq)

instance Show RawType where
  show RTInt = "Int"
  show RTStr = "String"
  show RTBool = "Bool"

fromType :: Type -> RawType
fromType (TInt _) = RTInt
fromType (TStr _) = RTStr
fromType (TBool _) = RTBool

fromArg :: Arg -> RawType
fromArg (VarArg _ argType _) = fromType argType
fromArg (RefArg _ argType _) = fromType argType

emptyEnv :: Env
emptyEnv = Env
  { types = M.empty,
    funcs = M.empty,
    retT = RTInt,
    isInsideLoop = False
  }


-- Methods related to types --
getVarType :: Ident -> Env -> Maybe RawType
getVarType ident Env{..} = M.lookup ident types

putType :: Ident -> RawType -> Env -> Env
putType ident newType env@Env{ types = oldTypes } = env { types = M.insert ident newType oldTypes }


-- Methods related to functions --
getFunc :: Ident -> Env -> Maybe (RawType, [Arg], Block)
getFunc ident Env{ funcs } = M.lookup ident funcs

putFunc :: Ident -> (RawType, [Arg], Block) -> Env -> Env
putFunc ident newFunc env@Env{ funcs = oldFuncs } = env { funcs = M.insert ident newFunc oldFuncs }

isFunc :: Ident -> Env -> Bool
isFunc ident Env{ funcs } = M.member ident funcs


-- Methods related to return type --
getRetType :: Env -> RawType
getRetType Env{ retT } = retT

setRetType :: RawType -> Env ->  Env
setRetType newRetT env@Env{ retT = _ } = env { retT = newRetT }


-- Methods related to loop flag --
getInsideLoop :: Env -> Bool
getInsideLoop Env{ isInsideLoop } = isInsideLoop

setInsideLoop :: Bool -> Env -> Env
setInsideLoop newFlag env@Env{ isInsideLoop = _ } = env { isInsideLoop = newFlag }
