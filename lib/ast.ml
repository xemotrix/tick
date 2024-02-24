open Sexplib.Std

type type' =
  | Int
  | Float
  | Bool
  | String
  | Char
  | Pointer of type'
[@@deriving show, sexp, eq]

type value =
  | ILiteral of int
  | FPLiteral of float
  | Var of string
  | BoolLiteral of bool
  | StringLiteral of string
[@@deriving show, sexp, eq]

type binop =
  | Mul
  | Div
  | Modulo
  | LogAnd
  | LogOr
  | LogXor
  | Concat
  | Add
  | Sub
  | Eq
  | Lt
  | Gt
[@@deriving show, sexp, eq]

type unop =
  | Deref
  | Ref
[@@deriving show, sexp, eq]

type expression =
  | BinOp of binop * expression * expression
  | UnOp of unop * expression
  | FunCall of string * expression list
  | Value of value
[@@deriving show, sexp, eq]

type statement =
  | Assign of string * expression
  | FunDef of string * (string * type') list * type' * block
  | If of (expression * block) list * block option
  | Return of expression
  | Print of expression
[@@deriving show, sexp, eq]

and block = Block of statement list [@@deriving show, sexp]

type root = block [@@deriving show, sexp]
