type t =
  | Eof
  (* Keywords *)
  | Fun
  | Let
  | Print
  | Extern
  | Return
  | If
  | Else
  | True
  | False
  (* Types *)
  | Int
  | Float
  | Bool
  (* Symbols *)
  (* Punctuation *)
  | Colon
  | Scln
  | Comma
  (* Grouping *)
  | LBrace
  | RBrace
  | LParen
  | RParen
  | Equals
  (* Binops *)
  | EqualsEq
  | Plus
  | Minus
  | Times
  | Divide
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | LogAnd
  | LogOr
  | LogXor
  | Modulo
  (* Values *)
  | Identifier of string
  | IntLiteral of int
  | FloatLiteral of float
[@@deriving show, eq]
