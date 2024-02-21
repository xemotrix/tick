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
  let item : Token.t Parser.t =
    fun ts ->
    match ts with
    | [] -> None
    | hd :: tl -> Some (hd, tl)
  ;;

  let zero : 'a Parser.t = fun _ -> None

  let sat (p : Token.t -> bool) : Token.t Parser.t =
    item >>= fun token -> if p token then return token else zero
  ;;

  let token t = sat Token.(equal t)

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
let number_int : int Parser.t = function
  | Token.IntLiteral n :: rest -> Some (n, rest)
  | _ -> None
;;

let number_float : float Parser.t = function
  | Token.FloatLiteral n :: rest -> Some (n, rest)
  | _ -> None
;;

let bool' : bool Parser.t = function
  | Token.True :: rest -> Some (true, rest)
  | Token.False :: rest -> Some (false, rest)
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
  | Token.Ref :: rest ->
    let x =
      match type' rest with
      | Some (t, rest') -> Some (Ast.Pointer t, rest')
      | None -> None
    in
    x
  | _ -> None
;;

let value : Ast.value Parser.t =
  let int' = number_int >>| fun n -> Ast.ILiteral n in
  let float' = number_float >>| fun n -> Ast.FPLiteral n in
  let bool' = bool' >>| fun b -> Ast.BoolLiteral b in
  let literal = bool' <|> int' <|> float' in
  let identifier = identifier >>| fun i -> Ast.Var i in
  literal <|> identifier
;;

let rec term_binop tok to_ast =
  let%bind f = factor in
  let%bind _ = token tok in
  let%bind t = term in
  return @@ to_ast (f, t)

and term ts =
  ts
  |>
  let mul = term_binop Token.Times (fun (f, t) -> Ast.Mul (f, t)) in
  let div = term_binop Token.Divide (fun (f, t) -> Ast.Div (f, t)) in
  let mod' = term_binop Token.Modulo (fun (f, t) -> Ast.Modulo (f, t)) in
  let or' = term_binop Token.LogOr (fun (f, t) -> Ast.LogOr (f, t)) in
  let and' = term_binop Token.LogAnd (fun (f, t) -> Ast.LogAnd (f, t)) in
  let xor' = term_binop Token.LogXor (fun (f, t) -> Ast.LogXor (f, t)) in
  let factor = factor >>| fun f -> Ast.Factor f in
  or' <|> and' <|> xor' <|> mod' <|> mul <|> div <|> factor

and expr_binop tok to_ast =
  let%bind t = term in
  let%bind _ = token tok in
  let%bind e = expr in
  return @@ to_ast (t, e)

and expr ts =
  ts
  |>
  let add = expr_binop Token.Plus (fun (t, e) -> Ast.Add (t, e)) in
  let sub = expr_binop Token.Minus (fun (t, e) -> Ast.Sub (t, e)) in
  let eq = expr_binop Token.EqualsEq (fun (t, e) -> Ast.Eq (t, e)) in
  let lt = expr_binop Token.Less (fun (t, e) -> Ast.Lt (t, e)) in
  let gt = expr_binop Token.Greater (fun (t, e) -> Ast.Gt (t, e)) in
  let term = term >>| fun t -> Ast.Term t in
  eq <|> add <|> sub <|> lt <|> gt <|> term

and factor_unaryop tok to_ast =
  let%bind _ = token tok in
  let%bind f = factor in
  return @@ to_ast f

and factor ts =
  ts
  |>
  let gexpr =
    let%bind _ = token Token.LParen in
    let%bind e = expr in
    let%bind _ = token Token.RParen in
    return (Ast.Group e)
  in
  let value = value >>| fun v -> Ast.Value v in
  let ref = factor_unaryop Token.Ref (fun t -> Ast.Ref t) in
  let deref = factor_unaryop Token.Deref (fun t -> Ast.Deref t) in
  let funcall =
    let%bind id = identifier in
    let%bind _ = token Token.LParen in
    let%bind args = some expr in
    let%bind _ = token Token.RParen in
    return (Ast.FunCall (id, args))
  in
  funcall <|> gexpr <|> value <|> ref <|> deref
;;

let assign =
  let%bind _ = token Token.Let in
  let%bind id = identifier in
  let%bind _ = token Token.Equals in
  let%bind e = expr in
  let%bind _ = token Token.Scln in
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
  return @@ Ast.FunDef (id, args, body, ret_t)

and print =
  let%bind _ = token Token.Print in
  let%bind e = expr in
  let%bind _ = token Token.Scln in
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
(* return @@ Ast.If ([ e, b ] @ elifs, None) *)

and return' =
  let%bind _ = token Token.Return in
  let%bind e = expr in
  let%bind _ = token Token.Scln in
  return @@ Ast.Return e

and stmt ts = ts |> (assign <|> fundef <|> print <|> if' <|> return')
and block ts = ts |> (some stmt >>| fun ss -> Ast.Block ss)

let parse = block
