open Sexplib.Std

type type' =
  | Int
  | Float
  | Bool
  | String
  | Pointer of type'
[@@deriving show, sexp, eq]

type value =
  | ILiteral of int
  | FPLiteral of float
  | Var of string
  | BoolLiteral of bool
  | StringLiteral of string
[@@deriving show, sexp, eq]

and factor =
  | Group of expression
  | FunCall of string * expression list
  | Value of value
  | Ref of factor
  | Deref of factor
[@@deriving show, sexp, eq]

and term =
  | Mul of factor * term
  | Div of factor * term
  | Modulo of factor * term
  | LogAnd of factor * term
  | LogOr of factor * term
  | LogXor of factor * term
  | Factor of factor
[@@deriving show, sexp, eq]

and expression =
  | Add of term * expression
  | Sub of term * expression
  | Eq of term * expression
  | Lt of term * expression
  | Gt of term * expression
  | Term of term
[@@deriving show, sexp, eq]

type statement =
  | Assign of string * expression
  | FunDef of string * (string * type') list * block * type'
  | If of (expression * block) list * block option
  | Return of expression
  | Print of expression
[@@deriving show, sexp, eq]

and block = Block of statement list [@@deriving show, sexp]

type root = block [@@deriving show, sexp]
