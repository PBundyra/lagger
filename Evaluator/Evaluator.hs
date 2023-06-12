{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Evaluator.Evaluator
    ( evalProgram
    ) where

import Common.Utils
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Evaluator.Exceptions
import Evaluator.Monad
import Evaluator.State
import Evaluator.Utils
import Lagger.Abs


evalProgram :: Program -> IO (Either RuntimeException ())
evalProgram program =
  runExceptT $ evalStateT (eval program) emptyState


instance Evaluator Program where

  eval (PProgram position topDefs) = do
    mapM_ eval topDefs
    void $ evalVal $ EApp position (Ident "main") []


instance Evaluator TopDef where

  eval (FnDef _ _ ident args block) = do
    (State env _ _) <- get
    let newFun = VFunc args block env
    modify $ putVal ident newFun


instance Evaluator Block where

  eval (BBlock _ stmts) = do
    (State envSnapshot _ _) <- get
    mapM_ evalIfNoFlags stmts
    modify $ putEnv envSnapshot     -- przywracamy środowiska do stanu sprzed bloku


instance Evaluator Stmt where
  
  eval (Empty _) = pure ()
  
  eval (BStmt _ block) = eval block
  
  eval (Decl _ tType (NoInit _ ident)) = do
    let val = defaultVal tType
    modify $ putVal ident val

  eval (Decl _ _ (Init _ ident expr)) = do
    val <- evalVal expr
    modify $ putVal ident val

  eval (DeclFunc _ fun) = eval fun
  
  eval (Ass _ ident expr) = do
    val <- evalVal expr
    modify $ updateVal ident val

  eval (Ret _ expr) = do
    val <- evalVal expr
    modify $ setRetVal val

  eval (IfCond _ cond block) = do
    condVal <- evalVal cond
    if isTrue condVal then eval block else pure ()

  eval (CondElse _ cond ifBlock elseBlock) = do
    condVal <- evalVal cond
    if isTrue condVal then eval ifBlock else eval elseBlock
  
  eval (While pos cond block) = do
    condVal <- evalVal cond
    fBreak <- gets isFBreakSet
    modify $ resetBreakFlag
    modify $ resetContFlag
    if isTrue condVal && (not fBreak)
    then do
     eval block
     void $ eval (While pos cond block)
    else pure ()

  eval (SExp _ expr) = void $ evalVal expr

  eval (Break _) = modify $ setBreakFlag

  eval (Continue _) = modify $ setContFlag


instance Evaluator Expr where
  
  evalVal (EVar _ ident) = do
    val <- gets $ getVal ident
    pure val
  
  evalVal (ELitInt _ val) = pure $ VInt val
  
  evalVal (ELitTrue _) = pure $ VBool True
  
  evalVal (ELitFalse _) = pure $ VBool False
  
  evalVal (EString _ val) = pure $ VString val
  
  evalVal (EApp pos ident exprs) = predefinedOrElse pos ident exprs $ do
    (State envSnapshot _ _) <- get
    (VFunc args block env) <- gets $ getVal ident
    vals <- mapM evalVal exprs

    -- ustawiam odpowiednio srodowisko
    -- obliczam lokacje zmiennych ktore przekazuje przez referencje
    locs <- mapM (\(arg, expr) -> getArgLoc arg expr) (zip args exprs)
    modify $ putEnv env                             -- przekazuje enva z funkcji do monady
    modify $ putVal ident (VFunc args block env)    -- dodaje sama funkcje zeby zapewnic rekurencje
     -- uzupelniam srodowisko funkcji o przekazane zmienne
    mapM_ (\(arg,val,loc) -> passArgToFunc arg val loc) (zip3 args vals locs)

    -- obliczam funkcje
    eval block
    retVal <- getRetVal

    -- przywracam środowisko do stanu sprzed wywołania funkcji
    modify $ putEnv envSnapshot
    modify $ resetRetFlag

    pure retVal

  evalVal (ENeg _ expr) = do
    (VInt val) <- evalVal expr
    pure $ VInt ((-1) * val)
  
  evalVal (ENot _ expr) = do
    (VBool val) <- evalVal expr
    pure $ VBool (not val)
  
  evalVal (EMul _ expr1 (Times _) expr2) = mapExprs (*) intToVInt vIntToInt expr1 expr2
  
  evalVal (EMul pos expr1 (Div _) expr2) = do
    (VInt val2) <- evalVal expr2
    if val2 == 0
      then throwError $ ArithmeticException pos
      else mapExprs quot intToVInt vIntToInt expr1 expr2
  
  evalVal (EMul pos expr1 (Mod _) expr2) = do
    (VInt val2) <- evalVal expr2
    if val2 == 0
      then throwError $ ArithmeticException pos
      else mapExprs mod intToVInt vIntToInt expr1 expr2
  
  evalVal (EAdd _ expr1 (Plus _) expr2) = do
    val1 <- evalVal expr1
    val2 <- evalVal expr2
    case (val1, val2) of
      (VInt v1, VInt v2) -> pure $ VInt (v1 + v2)
      (VString v1, VString v2) -> pure $ VString (v1 ++ v2)
  
  evalVal (EAdd _ expr1 (Minus _) expr2) = mapExprs (-) intToVInt vIntToInt expr1 expr2
  
  evalVal (ERel _ expr1 (LTH _) expr2) = mapExprs (<) boolToVBool vIntToInt expr1 expr2
  
  evalVal (ERel _ expr1 (LE _) expr2) = mapExprs (<=) boolToVBool vIntToInt expr1 expr2
  
  evalVal (ERel _ expr1 (GTH _) expr2) = mapExprs (>) boolToVBool vIntToInt expr1 expr2
  
  evalVal (ERel _ expr1 (GE _) expr2) = mapExprs (>=) boolToVBool vIntToInt expr1 expr2
  
  evalVal (ERel _ expr1 (EQU _) expr2) = do
    val1 <- evalVal expr1
    val2 <- evalVal expr2
    pure $ VBool $ val1 == val2
  
  evalVal (ERel _ expr1 (NE _) expr2) = do
    val1 <- evalVal expr1
    val2 <- evalVal expr2
    pure $ VBool $ val1 /= val2
  
  evalVal (EAnd _ expr1 expr2) = mapExprs (&&) boolToVBool vBoolToBool expr1 expr2
  
  evalVal (EOr _ expr1 expr2) = mapExprs (||) boolToVBool vBoolToBool expr1 expr2


getArgLoc :: Arg -> Expr -> EvaluatorM' (Maybe Loc)
getArgLoc (VarArg _ _ _) _ = pure Nothing
getArgLoc (RefArg _ _ _) (EVar _ ident) = do
    state <- get
    pure $ Just $ getLoc ident state

passArgToFunc :: Arg -> Value -> Maybe Loc -> EvaluatorStmt
passArgToFunc (VarArg _ _ ident) val _ = modify $ putVal ident val

passArgToFunc (RefArg _ _ ident) _ loc = do
  modify $ putLoc ident $ fromJust loc


mapExprs :: (a -> a -> b) -> (b -> Value) -> (Value -> a) -> Expr -> Expr -> EvaluatorM
mapExprs f wrap unwrap expr1 expr2 = do
  val1 <- evalVal expr1
  val2 <- evalVal expr2
  pure $ wrap $ f (unwrap val1) (unwrap val2)

evalIfNoFlags :: Stmt -> EvaluatorStmt
evalIfNoFlags stmt = do
  isRetSet <- gets isFRetSet
  isBreakSet <- gets isFBreakSet
  isContSet <- gets isFContSet
  if isRetSet || isContSet || isBreakSet then pure () else eval stmt

predefinedOrElse :: BNFC'Position -> Ident -> [Expr] -> EvaluatorM -> EvaluatorM
predefinedOrElse pos ident exprs f = do
  vals <- mapM evalVal exprs
  if isPredefined ident then execPredefined pos ident vals else f
