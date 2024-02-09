open Core

module Parser = struct
  type 'a t = Token.t list -> ('a * Token.t list) option

  let and_then (parser : 'a t) (other : 'b t) : ('a * 'b) t =
    fun input ->
    match parser input with
    | Some (result, rest) ->
      (match other rest with
       | Some (result', rest') -> Some ((result, result'), rest')
       | None -> None)
    | None -> None
  ;;

  let either (parser : 'a t) (other : 'a t) : 'a t =
    fun ts ->
    match parser ts with
    | Some (result, rest) -> Some (result, rest)
    | None ->
      (match other ts with
       | Some (result, rest) -> Some (result, rest)
       | None -> None)
  ;;

  let lignore (parser : unit t) (other : 'a t) : 'a t =
    fun ts ->
    match and_then parser other ts with
    | Some ((_, x), rest) -> Some (x, rest)
    | None -> None
  ;;

  let rignore (parser : 'a t) (other : unit t) : 'a t =
    fun ts ->
    match and_then parser other ts with
    | Some ((x, _), rest) -> Some (x, rest)
    | None -> None
  ;;

  let ignore (parser : 'a t) (other : unit t) : 'a t =
    fun ts ->
    match and_then parser other ts with
    | Some ((x, _), rest) -> Some (x, rest)
    | None -> None
  ;;

  let map (parser : 'a t) ~(f : 'a -> 'b) : 'b t =
    fun ts ->
    match parser ts with
    | Some (result, rest) -> Some (f result, rest)
    | None -> None
  ;;

  let merge (parser : 'a t) (other : 'a t) : 'a list t =
    fun ts ->
    match parser ts with
    | Some (result, rest) ->
      (match other rest with
       | Some (result', rest) -> Some ([ result'; result ], rest)
       | None -> None)
    | None -> None
  ;;

  let bind (parser : 'a list t) (other : 'a t) : 'a list t =
    fun ts ->
    match parser ts with
    | Some (result, rest) ->
      (match other rest with
       | Some (result', rest') -> Some (result' :: result, rest')
       | None -> None)
    | None -> None
  ;;

  let maybe (parser : 'a t) : 'a option t =
    fun ts ->
    match parser ts with
    | Some (result, rest) -> Some (Some result, rest)
    | None -> Some (None, ts)
  ;;

  let some (parser : 'a t) : 'a list t =
    let rec some' (parser : 'a t) (acc : 'a list) : 'a list t =
      fun ts ->
      match parser ts with
      | Some (result, rest) -> some' parser (result :: acc) rest
      | None -> if List.is_empty acc then None else Some (List.rev acc, ts)
    in
    some' parser []
  ;;

  let ( <&> ) = and_then
  let ( <|> ) = either
  let ( &> ) = lignore
  let ( <& ) = rignore
  let ( >>| ) a f = map a ~f
  let ( >>= ) a f = bind a f
  let ( =<< ) = Fn.flip bind
  let ( <&< ) a b = merge a b
end

include Parser

let value : Ast.value Parser.t =
  fun ts ->
  let open Ast in
  match ts with
  | Token.Number n :: rest -> Some (Literal n, rest)
  | Token.Identifier id :: rest -> Some (Var id, rest)
  | _ -> None
;;

let consume token : unit Parser.t =
  fun ts ->
  match ts with
  | t :: rest when Token.equal t token -> Some ((), rest)
  | _ -> None
;;

let rec term : Ast.term Parser.t =
  fun ts ->
  let mul = factor <& consume Token.Times <&> term >>| Ast.mul in
  let div = factor <& consume Token.Divide <&> term >>| Ast.div in
  let factor = factor >>| Ast.factor in
  (mul <|> div <|> factor) ts

and expr : Ast.expression Parser.t =
  fun ts ->
  let add = term <& consume Token.Plus <&> expr >>| Ast.add in
  let sub = term <& consume Token.Minus <&> expr >>| Ast.sub in
  let lt = term <& consume Token.Less <&> expr >>| Ast.lt in
  let gt = term <& consume Token.Greater <&> expr >>| Ast.gt in
  let term = term >>| Ast.term in
  (gt <|> lt <|> add <|> sub <|> term) ts

and factor : Ast.factor Parser.t =
  fun ts ->
  let gexpr = consume Token.LParen &> expr <& consume Token.RParen >>| Ast.group in
  let value = value >>| Ast.value in
  let funcall =
    identifier
    <& consume Token.LParen
    <&> some expr
    <& consume Token.RParen
    >>| Ast.fun_call
  in
  (funcall <|> gexpr <|> value) ts

and identifier : string Parser.t = function
  | Token.Identifier i :: rest -> Some (i, rest)
  | _ -> None
;;

let rec block : Ast.block Parser.t = fun ts -> (some stmt >>| Ast.block) ts

and assign : Ast.statement Parser.t =
  consume Token.Let
  &> identifier
  <& consume Token.Equals
  <&> expr
  <& consume Token.Scln
  >>| Ast.assign

and fundef : Ast.statement Parser.t =
  fun ts ->
  (consume Token.Fun
   &> identifier
   <& consume Token.LParen
   <&> some identifier
   <& consume Token.RParen
   <& consume Token.LBrace
   <&> block
   <& consume Token.RBrace
   >>| Ast.fun_def)
    ts

and print : Ast.statement Parser.t =
  consume Token.Print &> expr <& consume Token.Scln >>| Ast.print

and if_stmt : Ast.statement Parser.t =
  fun ts ->
  let basic_if =
    consume Token.If &> expr <& consume Token.LBrace <&> block <& consume Token.RBrace
  in
  (basic_if >>| Ast.if_stmt) ts

and return : Ast.statement Parser.t =
  consume Token.Return &> expr <& consume Token.Scln >>| Ast.return

and stmt : Ast.statement Parser.t =
  fun ts -> (if_stmt <|> return <|> fundef <|> assign <|> print) ts
;;

let parse = block
