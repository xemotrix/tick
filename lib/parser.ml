open Core

module Parser = struct
  type 'a t = Token.t list -> ('a * Token.t list) option

  let return (x : 'a) : 'a t = fun ts -> Some (x, ts)

  let bind (parser : 'a t) ~(f : 'a -> 'b t) : 'b t =
    fun ts ->
    match parser ts with
    | None -> None
    | Some (x, ts') -> f x ts'
  ;;

  let map = `Define_using_bind
end

module MParser = Monad.Make (Parser)
open MParser.Let_syntax

module Combinators = struct
  let zero : 'a Parser.t = fun _ -> None

  let token t = function
    | hd :: tl when Token.equal hd t -> Some (hd, tl)
    | _ -> None
  ;;

  let or' (p : 'a Parser.t) (q : 'a Parser.t) : 'a Parser.t =
    fun ts ->
    match p ts with
    | Some (x, rest) -> Some (x, rest)
    | None -> q ts
  ;;

  let ( <|> ) = or'

  let rec some p =
    (let%bind x = p in
     let%bind xs = some p in
     return (x :: xs))
    <|> return []
  ;;

  let maybe p =
    (let%bind x = p in
     return (Some x))
    <|> return None
  ;;
end

open Combinators

(* Tick parsing *)
(* ------------ *)
let int_literal : Ast.value Parser.t = function
  | Token.IntLiteral n :: rest -> Some (Ast.ILiteral n, rest)
  | _ -> None
;;

let float_literal : Ast.value Parser.t = function
  | Token.FloatLiteral n :: rest -> Some (Ast.FPLiteral n, rest)
  | _ -> None
;;

let string_literal : Ast.value Parser.t = function
  | Token.StringLiteral s :: rest -> Some (Ast.StringLiteral s, rest)
  | _ -> None
;;

let bool_literal : Ast.value Parser.t = function
  | Token.True :: rest -> Some (Ast.BoolLiteral true, rest)
  | Token.False :: rest -> Some (Ast.BoolLiteral false, rest)
  | _ -> None
;;

let identifier : string Parser.t = function
  | Token.Identifier s :: rest -> Some (s, rest)
  | _ -> None
;;

let rec type' : Ast.type' Parser.t = function
  | Token.Int :: rest -> Some (Ast.Int, rest)
  | Token.Float :: rest -> Some (Ast.Float, rest)
  | Token.Bool :: rest -> Some (Ast.Bool, rest)
  | Token.String :: rest -> Some (Ast.String, rest)
  | Token.Ref :: rest ->
    let x =
      match type' rest with
      | Some (t, rest') -> Some (Ast.Pointer t, rest')
      | None -> None
    in
    x
  | _ -> None
;;

let token_of_binop = function
  | Ast.Mul -> Token.Times
  | Ast.Div -> Token.Divide
  | Ast.Modulo -> Token.Modulo
  | Ast.Concat -> Token.StrConcat
  | Ast.Add -> Token.Plus
  | Ast.Sub -> Token.Minus
  | Ast.Lt -> Token.Less
  | Ast.Gt -> Token.Greater
  | Ast.Eq -> Token.EqualsEq
  | Ast.LogAnd -> Token.LogAnd
  | Ast.LogXor -> Token.LogXor
  | Ast.LogOr -> Token.LogOr
;;

let token_of_unop = function
  | Ast.Ref -> Token.Ref
  | Ast.Deref -> Token.Deref
;;

type assoc =
  | Left
  | Right

type precedence =
  | Unit
  | Access
  | Memory
  | Multiplicative
  | Additive
  | Comparison
  | Equality
  | LogicalAnd
  | LogicalOr
  | TopPrecedence
[@@deriving sexp, eq, ord, enum]

let prev_prec p =
  let open Option.Let_syntax in
  let%bind p = p in
  precedence_to_enum p |> Int.pred |> precedence_of_enum
;;

let rec field_assign ts =
  ts
  |>
  let%bind id = identifier in
  let%bind _ = token Token.Equals in
  let%bind e = expr in
  return (id, e)

and struct_literal ts =
  ts
  |>
  let%bind typeid = identifier in
  let%bind _ = token Token.LBrace in
  let%bind fields = some field_assign in
  let%bind _ = token Token.RBrace in
  return (Ast.StructLiteral (typeid, fields))

and value ts =
  ts
  |> (bool_literal
      <|> int_literal
      <|> float_literal
      <|> string_literal
      <|> struct_literal
      <|> (identifier >>| fun id -> Ast.Var id)
      >>| fun v -> Ast.Value v)

and binop ?(assoc = Left) prec op =
  let%bind l = precexpr (prev_prec prec) in
  let%bind _ = token (token_of_binop op) in
  let%bind r = precexpr prec in
  match assoc with
  | Left -> return @@ Ast.(BinOp (op, l, r))
  | Right -> return @@ Ast.(BinOp (op, r, l))

and unop prec op =
  let%bind _ = token (token_of_unop op) in
  let%bind f = precexpr prec in
  return @@ Ast.(UnOp (op, f))

and expr : Ast.expression Parser.t = function
  | ts -> precexpr (Some TopPrecedence) ts

and group_expr ts =
  ts
  |>
  let%bind _ = token Token.LParen in
  let%bind e = expr in
  let%bind _ = token Token.RParen in
  return e

and funcall ts =
  ts
  |>
  let%bind id = identifier in
  let%bind _ = token Token.LParen in
  let%bind args = some expr in
  let%bind _ = token Token.RParen in
  return (Ast.FunCall (id, args))

and access ts =
  ts
  |>
  let%bind struct_expr = precexpr (Some Unit) in
  let%bind _ = token Token.Period in
  let%bind field = identifier in
  return (Ast.Access (struct_expr, field))

and precexpr prec =
  match prec with
  | None -> zero
  | Some p ->
    let binop = binop prec in
    let unop = unop prec in
    (match p with
     | Unit -> funcall <|> group_expr <|> value
     | Access -> access
     | Memory -> unop Ref <|> unop Deref
     | Multiplicative -> binop Mul <|> binop Div <|> binop Modulo <|> binop Concat
     | Additive -> binop Add <|> binop Sub
     | Comparison -> binop Lt <|> binop Gt
     | Equality -> binop Eq
     | LogicalAnd -> binop LogAnd
     | LogicalOr -> binop LogOr
     | TopPrecedence -> zero)
    <|> precexpr (prev_prec prec)
;;

let assign =
  let%bind _ = token Token.Let in
  let%bind id = identifier in
  let%bind _ = token Token.Equals in
  let%bind e = expr in
  return @@ Ast.Assign (id, e)
;;

let typed_iden =
  let%bind id = identifier in
  let%bind _ = token Token.Colon in
  let%bind t = type' in
  return (id, t)
;;

let rec fundef ts =
  ts
  |>
  let%bind _ = token Token.Fun in
  let%bind id = identifier in
  let%bind _ = token Token.LParen in
  let%bind args = some typed_iden in
  let%bind _ = token Token.RParen in
  let%bind ret_t = type' in
  let%bind _ = token Token.LBrace in
  let%bind body = block in
  let%bind _ = token Token.RBrace in
  return @@ Ast.FunDef (id, args, ret_t, body)

and typedef ts =
  ts
  |>
  let%bind _ = token Token.Type in
  let%bind id = identifier in
  let%bind _ = token Token.LBrace in
  let%bind fields = some typed_iden in
  let%bind _ = token Token.RBrace in
  return @@ Ast.TypeDef (id, fields)

and print =
  let%bind _ = token Token.Print in
  let%bind e = expr in
  return @@ Ast.Print e

and braced_block ts =
  ts
  |>
  let%bind _ = token Token.LBrace in
  let%bind b = block in
  let%bind _ = token Token.RBrace in
  return b

and elseif' ts =
  ts
  |>
  let%bind _ = token Token.Else in
  let%bind _ = token Token.If in
  let%bind e = expr in
  let%bind b = braced_block in
  return @@ (e, b)

and else' ts =
  ts
  |>
  let%bind _ = token Token.Else in
  let%bind b = braced_block in
  return b

and if' ts =
  ts
  |>
  let%bind _ = token Token.If in
  let%bind e = expr in
  let%bind b = braced_block in
  let%bind elifs = some elseif' in
  let%bind el = maybe else' in
  return @@ Ast.If ([ e, b ] @ elifs, el)

and return' =
  let%bind _ = token Token.Return in
  let%bind e = expr in
  return @@ Ast.Return e

and stmt ts = ts |> (assign <|> fundef <|> typedef <|> print <|> if' <|> return')
and block ts = ts |> (some stmt >>| fun ss -> Ast.Block ss)

let parse = block
