-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Lagger.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Lagger.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Lagger.Abs.Ident -> Result
transIdent x = case x of
  Lagger.Abs.Ident string -> failure x

transProgram :: Show a => Lagger.Abs.Program' a -> Result
transProgram x = case x of
  Lagger.Abs.PProgram _ topdefs -> failure x

transTopDef :: Show a => Lagger.Abs.TopDef' a -> Result
transTopDef x = case x of
  Lagger.Abs.FnDef _ type_ ident args block -> failure x

transArg :: Show a => Lagger.Abs.Arg' a -> Result
transArg x = case x of
  Lagger.Abs.VarArg _ type_ ident -> failure x
  Lagger.Abs.RefArg _ type_ ident -> failure x

transBlock :: Show a => Lagger.Abs.Block' a -> Result
transBlock x = case x of
  Lagger.Abs.BBlock _ stmts -> failure x

transStmt :: Show a => Lagger.Abs.Stmt' a -> Result
transStmt x = case x of
  Lagger.Abs.Empty _ -> failure x
  Lagger.Abs.BStmt _ block -> failure x
  Lagger.Abs.Decl _ type_ item -> failure x
  Lagger.Abs.DeclFunc _ topdef -> failure x
  Lagger.Abs.Ass _ ident expr -> failure x
  Lagger.Abs.Ret _ expr -> failure x
  Lagger.Abs.IfCond _ expr block -> failure x
  Lagger.Abs.CondElse _ expr block1 block2 -> failure x
  Lagger.Abs.While _ expr block -> failure x
  Lagger.Abs.SExp _ expr -> failure x
  Lagger.Abs.Break _ -> failure x
  Lagger.Abs.Continue _ -> failure x

transItem :: Show a => Lagger.Abs.Item' a -> Result
transItem x = case x of
  Lagger.Abs.NoInit _ ident -> failure x
  Lagger.Abs.Init _ ident expr -> failure x

transType :: Show a => Lagger.Abs.Type' a -> Result
transType x = case x of
  Lagger.Abs.TInt _ -> failure x
  Lagger.Abs.TStr _ -> failure x
  Lagger.Abs.TBool _ -> failure x

transExpr :: Show a => Lagger.Abs.Expr' a -> Result
transExpr x = case x of
  Lagger.Abs.EVar _ ident -> failure x
  Lagger.Abs.ELitInt _ integer -> failure x
  Lagger.Abs.ELitTrue _ -> failure x
  Lagger.Abs.ELitFalse _ -> failure x
  Lagger.Abs.EApp _ ident exprs -> failure x
  Lagger.Abs.EString _ string -> failure x
  Lagger.Abs.EArrayEl _ ident expr -> failure x
  Lagger.Abs.EArrayLen _ ident -> failure x
  Lagger.Abs.ENeg _ expr -> failure x
  Lagger.Abs.ENot _ expr -> failure x
  Lagger.Abs.EMul _ expr1 mulop expr2 -> failure x
  Lagger.Abs.EAdd _ expr1 addop expr2 -> failure x
  Lagger.Abs.ERel _ expr1 relop expr2 -> failure x
  Lagger.Abs.EAnd _ expr1 expr2 -> failure x
  Lagger.Abs.EOr _ expr1 expr2 -> failure x

transAddOp :: Show a => Lagger.Abs.AddOp' a -> Result
transAddOp x = case x of
  Lagger.Abs.Plus _ -> failure x
  Lagger.Abs.Minus _ -> failure x

transMulOp :: Show a => Lagger.Abs.MulOp' a -> Result
transMulOp x = case x of
  Lagger.Abs.Times _ -> failure x
  Lagger.Abs.Div _ -> failure x
  Lagger.Abs.Mod _ -> failure x

transRelOp :: Show a => Lagger.Abs.RelOp' a -> Result
transRelOp x = case x of
  Lagger.Abs.LTH _ -> failure x
  Lagger.Abs.LE _ -> failure x
  Lagger.Abs.GTH _ -> failure x
  Lagger.Abs.GE _ -> failure x
  Lagger.Abs.EQU _ -> failure x
  Lagger.Abs.NE _ -> failure x
