open Sexplib.Std

type type' =
  | Int
  | Float
  | Bool
  | String
  | Char
  | Pointer of type'
  (* | Struct of string * (string * type') list *)
  | Struct of string 
[@@deriving show, sexp, eq]

and value =
  | ILiteral of int
  | FPLiteral of float
  | Var of string
  | BoolLiteral of bool
  | StringLiteral of string
  | StructLiteral of string * (string * expression) list
[@@deriving show, sexp, eq]

and binop =
  | Add
  | Concat
  | Div
  | Eq
  | Gt
  | LogAnd
  | LogOr
  | LogXor
  | Lt
  | Modulo
  | Mul
  | Sub
[@@deriving show, sexp, eq]

and unop =
  | Deref
  | Ref
[@@deriving show, sexp, eq]

and expression =
  | BinOp of binop * expression * expression
  | Access of expression * string
  | UnOp of unop * expression
  | FunCall of string * expression list
  | Value of value
[@@deriving show, sexp, eq]

and statement =
  | Assign of string * expression
  | FunDef of string * (string * type') list * type' * block
  | TypeDef of string * (string * type') list
  | If of (expression * block) list * block option
  | Return of expression
  | Print of expression
[@@deriving show, sexp, eq]

and block = Block of statement list [@@deriving show, sexp]
and root = block [@@deriving show, sexp]
