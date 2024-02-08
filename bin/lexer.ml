open Core

let next (s : string) : char option =
  if String.is_empty s then None else Some (String.get s 0)
;;

let peek (s : string) : char option =
  if String.length s < 2 then None else Some (String.get s 1)
;;

let lex_digit (input : string) : Token.t option * string =
  let open Char in
  let num_str = String.take_while input ~f:is_digit in
  match int_of_string_opt num_str with
  | None -> failwith "Invalid number representation"
  | Some num -> Some (Number num), String.drop_prefix input (String.length num_str)
;;

let advance (input : string) : string = String.drop_prefix input 1
let advance_n (input : string) (n : int) : string = String.drop_prefix input n

let get_iden (input : string) : string * string =
  let open Char in
  let iden =
    String.take_while input ~f:(fun c -> is_alpha c || is_digit c || equal c '_')
  in
  iden, advance_n input (String.length iden)
;;

let rec lex' ((input, tokens) : string * Token.t list) : Token.t list * string =
  if String.is_empty input
  then Eof :: tokens, ""
  else
    let module S = String in
    (match String.get input 0 with
     | c when Char.is_whitespace c -> advance input, tokens
     | ';' -> advance input, Scln :: tokens
     | ',' -> advance input, Comma :: tokens
     | '{' -> advance input, LBrace :: tokens
     | '}' -> advance input, RBrace :: tokens
     | '(' -> advance input, LParen :: tokens
     | ')' -> advance input, RParen :: tokens
     | '+' -> advance input, Plus :: tokens
     | '-' -> advance input, Minus :: tokens
     | '*' -> advance input, Times :: tokens
     | '/' ->
       (match peek input with
        | Some '/' ->
          input
          |> S.take_while ~f:(fun c -> not @@ Char.equal c '\n')
          |> S.length
          |> advance_n input
          |> fun rest -> rest, tokens
        | _ -> advance input, Divide :: tokens)
     | '=' ->
       (match peek input with
        | Some '=' -> advance_n input 2, EqualsEq :: tokens
        | _ -> advance input, Equals :: tokens)
     | '<' ->
       (match peek input with
        | Some '=' -> advance_n input 2, LessEq :: tokens
        | _ -> advance input, Less :: tokens)
     | '>' ->
       (match peek input with
        | Some '=' -> advance_n input 2, GreaterEq :: tokens
        | _ -> advance input, Greater :: tokens)
     | c when Char.is_digit c ->
       (match lex_digit input with
        | None, _ -> failwith "Invalid number representation"
        | Some num, rest -> rest, num :: tokens)
     | c when Char.is_alpha c ->
       (match get_iden input with
        | "fun", rest -> rest, Fun :: tokens
        | "extern", rest -> rest, Extern :: tokens
        | "if", rest -> rest, If :: tokens
        | "let", rest -> rest, Let :: tokens
        | "print", rest -> rest, Print :: tokens
        | "return", rest -> rest, Return :: tokens
        | iden, rest -> rest, Identifier iden :: tokens)
     | _ -> failwith @@ Printf.sprintf "Unknown token: '%s'" input)
    |> lex'
;;

let lex input =
  match lex' (input, []) with
  | tokens, "" -> List.rev tokens
  | _ -> failwith "Failed to lex whole input"
;;
