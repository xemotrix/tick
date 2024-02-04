open Sexplib.Std

type factor =
  | Group of expression
  | FunCall of string * expression list
  | Value of value
[@@deriving show, sexp]

and value =
  | Literal of int
  | Var of string
[@@deriving show, sexp]

and term =
  | Mul of factor * term
  | Div of factor * term
  | Factor of factor
[@@deriving show, sexp]

and expression =
  | Add of term * expression
  | Sub of term * expression
  | Lt of term * expression
  | Gt of term * expression
  | Term of term
[@@deriving show, sexp]

type statement =
  | Assign of string * expression
  | FunDef of string * string list * block
  | If of expression * block
  | Return of expression
  | Print of expression
[@@deriving show, sexp]

and block = Block of statement list [@@deriving show, sexp]

type root = block [@@deriving show, sexp]

(* factor *)
let group e = Group e
let fun_call (s, args) = FunCall (s, args)
let value v = Value v

(* value *)
let literal f = Literal f
let var s = Var s

(* term *)
let mul (f, t) = Mul (f, t)
let div (f, t) = Div (f, t)
let factor f = Factor f

(* expression *)
let add (t, e) = Add (t, e)
let sub (t, e) = Sub (t, e)
let lt (t, e) = Lt (t, e)
let gt (t, e) = Gt (t, e)
let term t = Term t

(* stmt *)
let assign (s, e) = Assign (s, e)
let print e = Print e
let return e = Return e
let if_stmt (e, block) = If (e, block)

let fun_def ((s, args), b) =
  print_endline (s ^ " " ^ String.concat " " args);
  FunDef (s, args, b)
;;

(* block *)
let block stmts = Block stmts
