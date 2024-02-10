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
  (* Symbols *)
  | Scln
  | Comma
  | LBrace
  | RBrace
  | LParen
  | RParen
  | Equals
  | EqualsEq
  | Plus
  | Minus
  | Times
  | Divide
  | Less
  | LessEq
  | Greater
  | GreaterEq
  (* Values *)
  | Identifier of string
  | Number of int
[@@deriving show, eq]
