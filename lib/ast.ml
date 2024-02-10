open Sexplib.Std

type value =
  | Literal of int
  | Var of string
  | True
  | False
[@@deriving show, sexp, eq]

and factor =
  | Group of expression
  | FunCall of string * expression list
  | Value of value
[@@deriving show, sexp, eq]

and term =
  | Mul of factor * term
  | Div of factor * term
  | Factor of factor
[@@deriving show, sexp, eq]

and expression =
  | Add of term * expression
  | Sub of term * expression
  | Lt of term * expression
  | Gt of term * expression
  | Term of term
[@@deriving show, sexp, eq]

type statement =
  | Assign of string * expression
  | FunDef of string * string list * block
  | If of (expression * block) list * block option
  | Return of expression
  | Print of expression
[@@deriving show, sexp, eq]

and block = Block of statement list [@@deriving show, sexp]

type root = block [@@deriving show, sexp]
