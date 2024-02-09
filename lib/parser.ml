open Core

module Parser = struct
  type 'a t = Token.t list -> ('a * Token.t list) list

  let return (x : 'a) : 'a t = fun tokens -> [ x, tokens ]

  let bind (parser : 'a t) ~(f : 'a -> 'b t) : 'b t =
    let open List.Monad_infix in
    fun tokens ->
      match parser tokens with
      | [] -> []
      | lst -> lst >>= fun (x, tokens') -> f x tokens'
  ;;

  let map = `Define_using_bind
end

module MParser = Monad.Make (Parser)
open MParser
open MParser.Let_syntax

let item : Token.t Parser.t =
  fun tokens ->
  match tokens with
  | [] -> []
  | hd :: tl -> [ hd, tl ]
;;

let zero : 'a Parser.t = fun _ -> []

let sat (p : Token.t -> bool) : Token.t Parser.t =
  item >>= fun token -> if p token then return token else zero
;;

let token t = sat Token.(equal t)

let plus (p : 'a Parser.t) (q : 'a Parser.t) : 'a Parser.t =
  fun tokens -> p tokens @ q tokens
;;

let or' (p : 'a Parser.t) (q : 'a Parser.t) : 'a Parser.t =
  fun tokens ->
  match plus p q tokens with
  | [] -> []
  | hd :: _ -> [ hd ]
;;

let ( <|> ) = or'

let rec some p =
  let%bind x = p in
  let%bind xs = some p in
  return (x :: xs) <|> return []
;;

let maybe p = p <|> return []

(* example *)
let ifelse =
  let%bind if' = token Token.If in
  let%bind else' = token Token.Else in
  let%bind what = return if' <|> return else' in
  let%bind wtf = some @@ return what in
  return (if', wtf)
;;
