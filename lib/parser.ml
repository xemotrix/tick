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
  let ( let* ) a f = MParser.bind a ~f
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

  let and' (p : 'a Parser.t) (q : 'b Parser.t) : ('a * 'b) Parser.t =
    let* l = p in
    let* r = q in
    return (l, r)
  ;;

  let and_left (p : 'a Parser.t) (q : 'b Parser.t) : 'a Parser.t =
    let%bind l = p in
    let%bind _ = q in
    return l
  ;;

  let and_right (p : 'a Parser.t) (q : 'b Parser.t) : 'b Parser.t =
    let%bind _ = p in
    let%bind r = q in
    return r
  ;;

  let ( <|> ) = or'
  let ( <<< ) = and_left
  let ( >>> ) = and_right
  let ( ~$ ) = token

  let rec some p =
    (let* x = p in
     let* xs = some p in
     return (x :: xs))
    <|> return []
  ;;

  let some_comma_separated p =
    (let* x = p in
     let* xs = some (~$Token.Comma >>> p) in
     return (x :: xs))
    <|> return []
  ;;

  let maybe p =
    (let* x = p in
     return @@ Some x)
    <|> return None
  ;;

  let to_list (p : 'a option Parser.t) =
    match%bind p with
    | Some x -> return [ x ]
    | None -> return []
  ;;
end

open Combinators

(* Tick parsing *)
(* ------------ *)

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

let rec int_literal : Ast.value Parser.t = function
  | Token.IntLiteral n :: rest -> Some (Ast.ILiteral n, rest)
  | _ -> None

and float_literal : Ast.value Parser.t = function
  | Token.FloatLiteral n :: rest -> Some (Ast.FPLiteral n, rest)
  | _ -> None

and string_literal : Ast.value Parser.t = function
  | Token.StringLiteral s :: rest -> Some (Ast.StringLiteral s, rest)
  | _ -> None

and bool_literal : Ast.value Parser.t = function
  | Token.True :: rest -> Some (Ast.BoolLiteral true, rest)
  | Token.False :: rest -> Some (Ast.BoolLiteral false, rest)
  | _ -> None

and identifier : string Parser.t = function
  | Token.Identifier s :: rest -> Some (s, rest)
  | _ -> None

and capIdentifier : string Parser.t = function
  | Token.CapIdentifier s :: rest -> Some (s, rest)
  | _ -> None

and int_t = function
  | Token.Int :: rest -> Some (Ast.Int, rest)
  | _ -> None

and float_t = function
  | Token.Float :: rest -> Some (Ast.Float, rest)
  | _ -> None

and bool_t = function
  | Token.Bool :: rest -> Some (Ast.Bool, rest)
  | _ -> None

and string_t = function
  | Token.String :: rest -> Some (Ast.String, rest)
  | _ -> None

and struct_t = function
  | Token.Identifier s :: rest -> Some (Ast.Struct s, rest)
  | _ -> None

and pointer_t = function
  | Token.Ref :: rest ->
    (match type' rest with
     | Some (t, rest') -> Some (Ast.Pointer t, rest')
     | None -> None)
  | _ -> None

and tuple_t ts =
  ts
  |>
  let* types = ~$Token.LParen >>> some_comma_separated type' <<< ~$Token.RParen in
  return @@ Ast.Tuple types

and type' ts =
  ts |> (int_t <|> float_t <|> bool_t <|> string_t <|> struct_t <|> pointer_t <|> tuple_t)

and token_of_binop = function
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

and token_of_unop = function
  | Ast.Ref -> Token.Ref
  | Ast.Deref -> Token.Deref

and prev_prec p =
  let open Option.Let_syntax in
  let%bind p = p in
  precedence_to_enum p |> Int.pred |> precedence_of_enum

and field_assign ts =
  ts
  |>
  let* id = identifier in
  let* _ = ~$Token.Equals in
  let* e = expr in
  return (id, e)

and tuple_literal ts =
  ts
  |>
  let* es = ~$Token.LParen >>> some_comma_separated expr <<< ~$Token.RParen in
  return @@ Ast.TupleLiteral es

and struct_literal ts =
  ts
  |>
  let* typeid = identifier in
  let* fields = ~$Token.LBrace >>> some field_assign <<< ~$Token.RBrace in
  return @@ Ast.StructLiteral (typeid, fields)

and value ts =
  ts
  |> (bool_literal
      <|> int_literal
      <|> float_literal
      <|> string_literal
      <|> struct_literal
      <|> tuple_literal
      <|> (identifier >>| fun id -> Ast.Var id)
      >>| fun v -> Ast.Value v)

and binop ?(assoc = Left) prec op =
  let* l = precexpr (prev_prec prec) in
  let* _ = ~$(token_of_binop op) in
  let* r = precexpr prec in
  match assoc with
  | Left -> return @@ Ast.(BinOp (op, l, r))
  | Right -> return @@ Ast.(BinOp (op, r, l))

and unop prec op =
  let* _ = ~$(token_of_unop op) in
  (* don't use >>> here because precexpr will recurse infinitely *)
  (* "precexpr prec" has to be inside a bind *)
  let* f = precexpr prec in
  return @@ Ast.(UnOp (op, f))

and expr : Ast.expression Parser.t = function
  | ts -> precexpr (Some TopPrecedence) ts

and group_expr ts = ts |> (~$Token.LParen >>> expr <<< ~$Token.RParen)

and funcall ts =
  ts
  |>
  let* id = identifier in
  let* args = ~$Token.LParen >>> some_comma_separated expr <<< ~$Token.RParen in
  return @@ Ast.FunCall (id, args)

and access ts =
  ts
  |>
  let* struct_expr = precexpr (Some Unit) in
  let* field = ~$Token.Period >>> identifier in
  return @@ Ast.Access (struct_expr, field)

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

and declaration ts =
  ts
  |>
  let* id = ~$Token.Let >>> identifier in
  let* e = ~$Token.Equals >>> expr in
  return @@ Ast.Declaration (id, e)

and assign ts =
  ts
  |>
  let* id = expr in
  let* e = ~$Token.Equals >>> expr in
  return @@ Ast.Assign (id, e)

and typed_iden ts =
  ts
  |>
  let* id = identifier in
  let* t = ~$Token.Colon >>> type' in
  return (id, t)

and fundef ts =
  ts
  |>
  let* id = ~$Token.Fun >>> identifier in
  let* args = ~$Token.LParen >>> some_comma_separated typed_iden <<< ~$Token.RParen in
  let* ret_t = type' in
  let* body = braced_block in
  return @@ Ast.FunDef (id, args, ret_t, body)

and typedef ts =
  ts
  |>
  let* id = ~$Token.Type >>> identifier in
  let* fields = ~$Token.LBrace >>> some typed_iden <<< ~$Token.RBrace in
  return @@ Ast.TypeDef (id, fields)

and enumdef ts =
  ts
  |>
  let* id = ~$Token.Enum >>> identifier in
  let variant =
    let* name = capIdentifier in
    let* typ = maybe type' in
    return (name, typ)
  in
  let* fields = ~$Token.LBrace >>> some variant <<< ~$Token.RBrace in
  return @@ Ast.EnumDef (id, fields)

and print ts =
  ts
  |>
  let* e = ~$Token.Print >>> expr in
  return @@ Ast.Print e

and braced_block ts = ts |> (~$Token.LBrace >>> block <<< ~$Token.RBrace)

and elseif' ts =
  ts
  |>
  let* e = ~$Token.Else >>> ~$Token.If >>> expr in
  let* b = braced_block in
  return @@ (e, b)

and else' ts =
  ts
  |>
  let* b = ~$Token.Else >>> braced_block in
  return b

and if' ts =
  ts
  |>
  let* e = ~$Token.If >>> expr in
  let* b = braced_block in
  let* elifs = some elseif' in
  let* el = maybe else' in
  return @@ Ast.If ([ e, b ] @ elifs, el)

and return' ts =
  ts
  |>
  let* e = ~$Token.Return >>> expr in
  return @@ Ast.Return e

and stmt ts =
  ts
  |> (declaration
      <|> assign
      <|> fundef
      <|> enumdef
      <|> typedef
      <|> print
      <|> if'
      <|> return')

and block ts = ts |> (some stmt >>| fun ss -> Ast.Block ss)

let parse = block
