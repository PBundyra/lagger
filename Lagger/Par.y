-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Lagger.Par
  ( happyError
  , myLexer
  , pProgram
  ) where

import Prelude

import qualified Lagger.Abs
import Lagger.Lex

}

%name pProgram_internal Program
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '!'        { PT _ (TS _ 1)  }
  '!='       { PT _ (TS _ 2)  }
  '%'        { PT _ (TS _ 3)  }
  '&'        { PT _ (TS _ 4)  }
  '&&'       { PT _ (TS _ 5)  }
  '('        { PT _ (TS _ 6)  }
  ')'        { PT _ (TS _ 7)  }
  '*'        { PT _ (TS _ 8)  }
  '+'        { PT _ (TS _ 9)  }
  ','        { PT _ (TS _ 10) }
  '-'        { PT _ (TS _ 11) }
  '.length'  { PT _ (TS _ 12) }
  '/'        { PT _ (TS _ 13) }
  ';'        { PT _ (TS _ 14) }
  '<'        { PT _ (TS _ 15) }
  '<='       { PT _ (TS _ 16) }
  '='        { PT _ (TS _ 17) }
  '=='       { PT _ (TS _ 18) }
  '>'        { PT _ (TS _ 19) }
  '>='       { PT _ (TS _ 20) }
  '['        { PT _ (TS _ 21) }
  ']'        { PT _ (TS _ 22) }
  'boolean'  { PT _ (TS _ 23) }
  'break'    { PT _ (TS _ 24) }
  'continue' { PT _ (TS _ 25) }
  'else'     { PT _ (TS _ 26) }
  'false'    { PT _ (TS _ 27) }
  'if'       { PT _ (TS _ 28) }
  'int'      { PT _ (TS _ 29) }
  'return'   { PT _ (TS _ 30) }
  'string'   { PT _ (TS _ 31) }
  'true'     { PT _ (TS _ 32) }
  'while'    { PT _ (TS _ 33) }
  '{'        { PT _ (TS _ 34) }
  '||'       { PT _ (TS _ 35) }
  '}'        { PT _ (TS _ 36) }
  L_Ident    { PT _ (TV _)    }
  L_integ    { PT _ (TI _)    }
  L_quoted   { PT _ (TL _)    }

%%

Ident :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Ident) }
Ident  : L_Ident { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Ident (tokenText $1)) }

Integer :: { (Lagger.Abs.BNFC'Position, Integer) }
Integer  : L_integ  { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), (read (tokenText $1)) :: Integer) }

String  :: { (Lagger.Abs.BNFC'Position, String) }
String   : L_quoted { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), ((\(PT _ (TL s)) -> s) $1)) }

Program :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Program) }
Program
  : ListTopDef { (fst $1, Lagger.Abs.PProgram (fst $1) (snd $1)) }

TopDef :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.TopDef) }
TopDef
  : Type Ident '(' ListArg ')' Block { (fst $1, Lagger.Abs.FnDef (fst $1) (snd $1) (snd $2) (snd $4) (snd $6)) }

ListTopDef :: { (Lagger.Abs.BNFC'Position, [Lagger.Abs.TopDef]) }
ListTopDef
  : TopDef { (fst $1, (:[]) (snd $1)) }
  | TopDef ListTopDef { (fst $1, (:) (snd $1) (snd $2)) }

Arg :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Arg) }
Arg
  : Type Ident { (fst $1, Lagger.Abs.VarArg (fst $1) (snd $1) (snd $2)) }
  | Type '&' Ident { (fst $1, Lagger.Abs.RefArg (fst $1) (snd $1) (snd $3)) }

ListArg :: { (Lagger.Abs.BNFC'Position, [Lagger.Abs.Arg]) }
ListArg
  : {- empty -} { (Lagger.Abs.BNFC'NoPosition, []) }
  | Arg { (fst $1, (:[]) (snd $1)) }
  | Arg ',' ListArg { (fst $1, (:) (snd $1) (snd $3)) }

Block :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Block) }
Block
  : '{' ListStmt '}' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.BBlock (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }

Stmt :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Stmt) }
Stmt
  : ';' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Empty (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | Block { (fst $1, Lagger.Abs.BStmt (fst $1) (snd $1)) }
  | Type Item { (fst $1, Lagger.Abs.Decl (fst $1) (snd $1) (snd $2)) }
  | TopDef { (fst $1, Lagger.Abs.DeclFunc (fst $1) (snd $1)) }
  | Ident '=' Expr ';' { (fst $1, Lagger.Abs.Ass (fst $1) (snd $1) (snd $3)) }
  | 'return' Expr ';' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Ret (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | 'if' '(' Expr ')' Block { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.IfCond (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | 'if' '(' Expr ')' Block 'else' Block { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.CondElse (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5) (snd $7)) }
  | 'while' '(' Expr ')' Block { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.While (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1)) (snd $3) (snd $5)) }
  | Expr ';' { (fst $1, Lagger.Abs.SExp (fst $1) (snd $1)) }
  | 'break' ';' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Break (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'continue' ';' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Continue (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }

Item :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Item) }
Item
  : Ident ';' { (fst $1, Lagger.Abs.NoInit (fst $1) (snd $1)) }
  | Ident '=' Expr ';' { (fst $1, Lagger.Abs.Init (fst $1) (snd $1) (snd $3)) }

ListStmt :: { (Lagger.Abs.BNFC'Position, [Lagger.Abs.Stmt]) }
ListStmt
  : {- empty -} { (Lagger.Abs.BNFC'NoPosition, []) }
  | Stmt ListStmt { (fst $1, (:) (snd $1) (snd $2)) }

Type :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Type) }
Type
  : 'int' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.TInt (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'string' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.TStr (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'boolean' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.TBool (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }

ListType :: { (Lagger.Abs.BNFC'Position, [Lagger.Abs.Type]) }
ListType
  : {- empty -} { (Lagger.Abs.BNFC'NoPosition, []) }
  | Type { (fst $1, (:[]) (snd $1)) }
  | Type ',' ListType { (fst $1, (:) (snd $1) (snd $3)) }

Expr6 :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Expr) }
Expr6
  : Ident { (fst $1, Lagger.Abs.EVar (fst $1) (snd $1)) }
  | Integer { (fst $1, Lagger.Abs.ELitInt (fst $1) (snd $1)) }
  | 'true' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.ELitTrue (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | 'false' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.ELitFalse (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | Ident '(' ListExpr ')' { (fst $1, Lagger.Abs.EApp (fst $1) (snd $1) (snd $3)) }
  | String { (fst $1, Lagger.Abs.EString (fst $1) (snd $1)) }
  | Ident '[' Expr ']' { (fst $1, Lagger.Abs.EArrayEl (fst $1) (snd $1) (snd $3)) }
  | Ident '.length' { (fst $1, Lagger.Abs.EArrayLen (fst $1) (snd $1)) }
  | Expr7 { (fst $1, (snd $1)) }

Expr5 :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Expr) }
Expr5
  : '-' Expr6 { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.ENeg (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | '!' Expr6 { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.ENot (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1)) (snd $2)) }
  | Expr6 { (fst $1, (snd $1)) }

Expr4 :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Expr) }
Expr4
  : Expr4 MulOp Expr5 { (fst $1, Lagger.Abs.EMul (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr5 { (fst $1, (snd $1)) }

Expr3 :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Expr) }
Expr3
  : Expr3 AddOp Expr4 { (fst $1, Lagger.Abs.EAdd (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr4 { (fst $1, (snd $1)) }

Expr2 :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Expr) }
Expr2
  : Expr2 RelOp Expr3 { (fst $1, Lagger.Abs.ERel (fst $1) (snd $1) (snd $2) (snd $3)) }
  | Expr3 { (fst $1, (snd $1)) }

Expr1 :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Expr) }
Expr1
  : Expr2 '&&' Expr1 { (fst $1, Lagger.Abs.EAnd (fst $1) (snd $1) (snd $3)) }
  | Expr2 { (fst $1, (snd $1)) }

Expr :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Expr) }
Expr
  : Expr1 '||' Expr { (fst $1, Lagger.Abs.EOr (fst $1) (snd $1) (snd $3)) }
  | Expr1 { (fst $1, (snd $1)) }

Expr7 :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.Expr) }
Expr7
  : '(' Expr ')' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), (snd $2)) }

ListExpr :: { (Lagger.Abs.BNFC'Position, [Lagger.Abs.Expr]) }
ListExpr
  : {- empty -} { (Lagger.Abs.BNFC'NoPosition, []) }
  | Expr { (fst $1, (:[]) (snd $1)) }
  | Expr ',' ListExpr { (fst $1, (:) (snd $1) (snd $3)) }

AddOp :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.AddOp) }
AddOp
  : '+' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Plus (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | '-' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Minus (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }

MulOp :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.MulOp) }
MulOp
  : '*' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Times (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | '/' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Div (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | '%' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.Mod (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }

RelOp :: { (Lagger.Abs.BNFC'Position, Lagger.Abs.RelOp) }
RelOp
  : '<' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.LTH (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | '<=' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.LE (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.GTH (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | '>=' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.GE (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | '==' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.EQU (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }
  | '!=' { (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1), Lagger.Abs.NE (uncurry Lagger.Abs.BNFC'Position (tokenLineCol $1))) }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

-- Entrypoints

pProgram :: [Token] -> Err Lagger.Abs.Program
pProgram = fmap snd . pProgram_internal
}

