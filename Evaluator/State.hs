{-# LANGUAGE RecordWildCards #-}
module Evaluator.State where

import Data.Maybe
import Lagger.Abs
import Prelude
import qualified Data.Map as M


data State = State
  { env :: Env,
    store :: Store,
    flags :: Flags
  }

type Env = M.Map Ident Loc
type Loc = Int

data Store = Store {mem :: M.Map Loc Value, freeLoc :: Int}

data Flags = Flags
  { fBreak :: Bool,
    fCont :: Bool,
    fRet :: Maybe (Value)
  }


data Value
  = VInt Integer
  | VBool Bool
  | VString String
  | VFunc [Arg] Block Env
  deriving (Eq)

instance Show Value where
    show (VInt val) = show val
    show (VBool val) = show val
    show (VString val) = show val

defaultVal :: Type -> Value
defaultVal (TInt _) = VInt 0
defaultVal (TBool _) = VBool False
defaultVal (TStr _) = VString ""


emptyState :: State
emptyState =
  State
    { env = emptyEnv,
      store = emptyStore,
      flags = emptyFlags
    }

emptyEnv :: Env
emptyEnv = M.empty

emptyStore :: Store
emptyStore = Store {mem = M.empty, freeLoc = 0}

emptyFlags :: Flags
emptyFlags =
  Flags
    { fBreak = False,
      fCont = False,
      fRet = Nothing
    }


-- Methods related to State --
getVal :: Ident -> State -> Value
getVal ident State {..} = getStoreVal loc store
  where
    loc = getEnvLoc ident env

putVal :: Ident -> Value -> State -> State
putVal ident val State {..} =
  State {env = newEnv, store = newStore, flags = flags}
  where
    (newLoc, newLocStore) = newloc store
    newStore = putStoreVal newLoc val newLocStore
    newEnv = M.insert ident newLoc env

updateVal :: Ident -> Value -> State -> State
updateVal ident val State {..} =
  State {env = env, store = newStore, flags = flags}
  where
    loc = getEnvLoc ident env
    newStore = putStoreVal loc val store

getLoc :: Ident -> State -> Loc
getLoc ident State {..} = getEnvLoc ident env

putLoc :: Ident -> Loc -> State -> State
putLoc ident loc State {..} =
  State {env = newEnv, store = store, flags = flags}
  where
    newEnv = M.insert ident loc env

putEnv :: Env -> State -> State
putEnv newEnv State {..} =
  State {env = newEnv, store = store, flags = flags}


-- Methods related to Store --
getStoreVal :: Loc -> Store -> Value
getStoreVal loc Store {..} = fromJust $ M.lookup loc mem

newloc :: Store -> (Loc, Store)
newloc Store {..} = (freeLoc, Store {mem = mem, freeLoc = freeLoc + 1})

putStoreVal :: Loc -> Value -> Store -> Store
putStoreVal loc val Store {..} = Store {mem = newMem, freeLoc = freeLoc}
  where
    newMem = M.insert loc val mem


-- Methods related to Env--
getEnvLoc :: Ident -> Env -> Loc
getEnvLoc ident env = fromJust $ M.lookup ident env


-- Methods related to flags --
setRetVal :: Value -> State -> State
setRetVal val State {..} =
  State {env = env, store = store, flags = newFlags}
  where
    newFlags = Flags {fCont = False, fBreak = False, fRet = Just val}

resetRetFlag :: State -> State
resetRetFlag State {..} =
  State {env = env, store = store, flags = newFlags}
  where
    newFlags = Flags {fCont = False, fBreak = False, fRet = Nothing}

isFRetSet :: State -> Bool
isFRetSet State {..} = not $ isNothing (fRet flags)

setBreakFlag :: State -> State
setBreakFlag State {..} =
  State {env = env, store = store, flags = newFlags}
  where
    newFlags = Flags {fCont = False, fBreak = True, fRet = fRet flags}

resetBreakFlag :: State -> State
resetBreakFlag State {..} =
  State {env = env, store = store, flags = newFlags}
  where
    newFlags = Flags {fCont = False, fBreak = False, fRet = fRet flags}

isFBreakSet :: State -> Bool
isFBreakSet State {..} = fBreak flags

setContFlag :: State -> State
setContFlag State {..} =
  State {env = env, store = store, flags = newFlags}
  where
    newFlags = Flags {fCont = True, fBreak = False, fRet = fRet flags}

resetContFlag :: State -> State
resetContFlag State {..} =
  State {env = env, store = store, flags = newFlags}
  where
    newFlags = Flags {fCont = False, fBreak = False, fRet = fRet flags}

isFContSet :: State -> Bool
isFContSet State {..} = fCont flags