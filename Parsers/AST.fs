module Parsers.AST

type sourceExpr =
    | Number of int
    | Mult of list<sourceExpr>
    | Add of list<sourceExpr>
    | Var of string

type sourceAst =
    | Asgn of string*sourceExpr
    | Print of sourceExpr

type targetExpr =
    | TNumber of int
    | TAdd of list<targetExpr>
    | TVar of string

type targetAst =
    | TStmt of string*targetExpr
