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
let number : int Parser.t = function
  | Token.Number n :: rest -> Some (n, rest)
  | _ -> None
;;

let identifier : string Parser.t = function
  | Token.Identifier s :: rest -> Some (s, rest)
  | _ -> None
;;

let value : Ast.value Parser.t =
  let literal = number >>| fun n -> Ast.Literal n in
  let identifier = identifier >>| fun i -> Ast.Var i in
  literal <|> identifier
;;

let rec term ts =
  ts
  |>
  let mul =
    let%bind f = factor in
    let%bind _ = token Token.Times in
    let%bind t = term in
    return (Ast.Mul (f, t))
  in
  let div =
    let%bind f = factor in
    let%bind _ = token Token.Divide in
    let%bind t = term in
    return (Ast.Div (f, t))
  in
  let factor = factor >>| fun f -> Ast.Factor f in
  mul <|> div <|> factor

and expr ts =
  ts
  |>
  let add =
    let%bind t = term in
    let%bind _ = token Token.Plus in
    let%bind e = expr in
    return (Ast.Add (t, e))
  in
  let sub =
    let%bind t = term in
    let%bind _ = token Token.Minus in
    let%bind e = expr in
    return (Ast.Sub (t, e))
  in
  let lt =
    let%bind t = term in
    let%bind _ = token Token.Less in
    let%bind e = expr in
    return (Ast.Lt (t, e))
  in
  let gt =
    let%bind t = term in
    let%bind _ = token Token.Greater in
    let%bind e = expr in
    return (Ast.Gt (t, e))
  in
  let term = term >>| fun t -> Ast.Term t in
  add <|> sub <|> lt <|> gt <|> term

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
  let funcall =
    let%bind id = identifier in
    let%bind _ = token Token.LParen in
    let%bind args = some expr in
    let%bind _ = token Token.RParen in
    return (Ast.FunCall (id, args))
  in
  funcall <|> gexpr <|> value
;;

let assign =
  let%bind _ = token Token.Let in
  let%bind id = identifier in
  let%bind _ = token Token.Equals in
  let%bind e = expr in
  let%bind _ = token Token.Scln in
  return @@ Ast.Assign (id, e)
;;

let rec fundef ts =
  ts
  |>
  let%bind _ = token Token.Fun in
  let%bind id = identifier in
  let%bind _ = token Token.LParen in
  let%bind args = some identifier in
  let%bind _ = token Token.RParen in
  let%bind _ = token Token.LBrace in
  let%bind body = block in
  let%bind _ = token Token.RBrace in
  return @@ Ast.FunDef (id, args, body)

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
