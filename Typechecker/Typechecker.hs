{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Typechecker.Typechecker
    ( checkTypeProgram
    ) where

import Control.Monad.Except
import Control.Monad.State
import Prelude
import Lagger.Abs
import Typechecker.Environment
import Typechecker.Exceptions
import Typechecker.Monad
import Typechecker.Utils


checkTypeProgram :: Program -> Either TypecheckingException ()
checkTypeProgram program =
  runExcept $ evalStateT (checkType program) emptyEnv

instance Typechecker Program where

  checkType (PProgram _ topDefs) = do
    assertHasMainFunc topDefs
    mapM_ checkType topDefs


instance Typechecker TopDef where

  checkType (FnDef pos tType ident args block) = do
    previousRetType <- gets getRetType  -- zapamietujemy poprzedni return type
    let retType = fromType tType

    -- Analiza sygnatury funkcji
    void $ assertIsNotPredefined pos ident -- upewniamy sie ze nie nazywa sie tak jak te predefiniowane
    void $ assertArgsAreUnique pos args    -- upewniamy sie ze argumenty nie maja tych samych nazw
    mapM_ (\arg -> modify $ putType (getArgIdent arg) (fromArg arg)) args -- dodajemy argumenty do srodowiska
    modify $ putFunc ident (retType, args, block)     --  dodajemy sama funkcje do srodowiska

    -- Analiza ciala funkcji
    modify $ setRetType retType         -- podmienieniamy return type
    isInsideLoop <- gets getInsideLoop  -- zapamietujemy czy jestesmy w petli
    modify $ setInsideLoop False        -- wchodzac do funkcji nie jestesmy w petli
    assertHasReturn pos block           -- upewniamy sie ze funkcja ma return
    checkType block

    -- Przywracamy srodowisko
    modify $ setInsideLoop isInsideLoop -- ustawiamy z powrotem czy jestesmy w petli
    modify $ setRetType previousRetType -- ustawiamy z powrotem return type


instance Typechecker Block where

  checkType (BBlock _ stmts) = do
    env <- get                          -- zapamietujemy srodowisko
    mapM_ checkType stmts
    put env                             -- przywracamy srodowisko


instance Typechecker Stmt where

  checkType (Empty _) = pure ()

  checkType (BStmt _ block) = checkType block

  checkType (Decl pos tType (NoInit _ ident)) = do
    assertIdentIsNotFunc pos ident
    let rawType = fromType tType
    modify $ putType ident rawType

  checkType (Decl pos tType (Init _ ident expr)) = do
    assertIdentIsNotFunc pos ident
    let rawType = fromType tType
    matchType pos rawType expr
    modify $ putType ident rawType

  checkType (DeclFunc _ fun) = checkType fun

  checkType (Ass pos ident expr) = do
    varType <- gets $ getVarType ident
    case varType of
      Just varType -> matchType pos varType expr
      Nothing -> throwError $ UndefinedReference pos ident

  checkType (Ret pos expr) = do
    retType <- gets getRetType
    matchType pos retType expr

  checkType (IfCond pos cond block) = do
    matchType pos RTBool cond
    checkType block

  checkType (CondElse pos cond ifBlock elseBlock) = do
    matchType pos RTBool cond
    checkType ifBlock
    checkType elseBlock

  checkType (While pos cond block) = do
    matchType pos RTBool cond
    isInsideLoop <- gets getInsideLoop      -- zapamietujemy czy jestesmy w petli
    modify $ setInsideLoop True
    checkType block
    modify $ setInsideLoop isInsideLoop

  checkType (SExp _ expr) = void $ getType expr

  checkType (Break pos) = assertIsInsideLoop pos

  checkType (Continue pos) = assertIsInsideLoop pos

instance Typechecker Expr where

  getType (EVar pos ident) = do
    varType <- gets $ getVarType ident
    case varType of
      Just t -> pure t
      Nothing -> throwError $ UndefinedReference pos ident


  getType (ELitInt _ _) = pure RTInt

  getType (ELitTrue _) = pure RTBool

  getType (ELitFalse _) = pure RTBool

  getType (EString _ _) = pure RTStr

  getType (EApp pos (Ident "println") exprs) = do
    assertIsCorrectNumOfArgs pos 1 exprs
    void $ getType (head exprs)
    pure RTInt

  getType (EApp pos (Ident "error") exprs) = do
    assertIsCorrectNumOfArgs pos 1 exprs
    matchType pos RTStr (head exprs)
    pure RTInt

  getType (EApp pos ident exprs) = do
    func <- gets $ getFunc ident
    case func of
      Just (retType, args, _) -> do
        assertIsCorrectNumOfArgs pos (length args) exprs                             -- czy liczba argumentow sie zgadza
        mapM_ (\(arg, expr) -> matchType pos (fromArg arg) expr) (zip args exprs)    -- czy typy sie zgadzaja
        mapM_ (\(arg, expr) -> assertIsValidReference pos arg expr) (zip args exprs) -- czy referencje sie zgadzaja
        pure retType
      Nothing -> throwError $ UndefinedReference pos ident

  getType (ENeg pos expr) = matchType pos RTInt expr >> pure RTInt

  getType (ENot pos expr) = matchType pos RTBool expr >> pure RTBool

  getType (EMul pos expr1 (Times _) expr2) = matchTypes pos RTInt expr1 expr2 >> pure RTInt

  getType (EMul pos expr1 (Div _) expr2) = matchTypes pos RTInt expr1 expr2 >> pure RTInt

  getType (EMul pos expr1 (Mod _) expr2) = matchTypes pos RTInt expr1 expr2 >> pure RTInt

  getType (EAdd pos expr1 (Plus _) expr2) = do
    type1 <- getType expr1
    type2 <- getType expr2
    case (type1, type2) of
      (RTInt , RTInt ) -> pure RTInt
      (RTStr, RTStr) -> pure RTStr
      (_, _) -> throwError $ TypeMismatch pos (type1, type2)

  getType (EAdd pos expr1 (Minus _) expr2) = matchTypes pos RTInt expr1 expr2 >> pure RTInt

  getType (ERel pos expr1 (LTH _) expr2) = matchTypes pos RTInt expr1 expr2 >> pure RTBool

  getType (ERel pos expr1 (LE _) expr2) = matchTypes pos RTInt expr1 expr2 >> pure RTBool

  getType (ERel pos expr1 (GTH _) expr2) = matchTypes pos RTInt expr1 expr2 >> pure RTBool

  getType (ERel pos expr1 (GE _) expr2) = matchTypes pos RTInt expr1 expr2 >> pure RTBool

  getType (ERel pos expr1 (EQU _) expr2) = do
    type1 <- getType expr1
    type2 <- getType expr2
    case (type1, type2) of
      (RTInt , RTInt ) -> pure RTBool
      (RTStr, RTStr) -> pure RTBool
      (RTBool, RTBool) -> pure RTBool
      (_, _) -> throwError $ TypeMismatch pos (type1, type2)

  getType (ERel pos expr1 (NE _) expr2) = getType (ERel pos expr1 (EQU pos) expr2)

  getType (EAnd pos expr1 expr2) = matchTypes pos RTBool expr1 expr2 >> pure RTBool

  getType (EOr pos expr1 expr2) = matchTypes pos RTBool expr1 expr2 >> pure RTBool


matchTypes :: BNFC'Position -> RawType -> Expr -> Expr -> TypecheckerM
matchTypes pos expectedType expr1 expr2 = do
  matchType pos expectedType expr1
  matchType pos expectedType expr2

matchType :: BNFC'Position -> RawType -> Expr -> TypecheckerM
matchType pos expectedType expr = do
  exprType <- getType expr
  assertTypesAreEqual pos expectedType exprType
