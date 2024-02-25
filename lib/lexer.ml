open Core

let next (s : string) : char option =
  if String.is_empty s then None else Some (String.get s 0)
;;

let peek (s : string) : char option =
  if String.length s < 2 then None else Some (String.get s 1)
;;

let lex_number (input : string) : Token.t option * string =
  let open Char in
  let num_str = String.take_while input ~f:(fun c -> is_digit c || equal c '.') in
  if String.contains num_str '.'
  then (
    match Float.of_string_opt num_str with
    | Some num ->
      Some (FloatLiteral num), String.drop_prefix input (String.length num_str)
    | None -> failwith "Invalid number representation")
  else (
    match int_of_string_opt num_str with
    | Some num -> Some (IntLiteral num), String.drop_prefix input (String.length num_str)
    | None -> failwith "Invalid number representation")
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

let sanitize_string (s : string) : string =
  let rec san acc s =
    match s with
    | '\\' :: 'n' :: rest -> san ('\n' :: acc) rest
    | '\\' :: 'r' :: rest -> san ('\r' :: acc) rest
    | '\\' :: 't' :: rest -> san ('\t' :: acc) rest
    | c :: rest -> san (c :: acc) rest
    | [] -> List.rev acc |> String.of_char_list
  in
  String.to_list s |> san []
;;

let rec lex' ((input, tokens) : string * Token.t list) : Token.t list * string =
  let module S = String in
  if S.is_empty input
  then Eof :: tokens, ""
  else
    (match String.get input 0 with
     | c when Char.is_whitespace c -> advance input, tokens
     | ';' -> advance input, Scln :: tokens
     | ':' -> advance input, Colon :: tokens
     | ',' -> advance input, Comma :: tokens
     | '.' -> advance input, Period :: tokens
     | '{' -> advance input, LBrace :: tokens
     | '}' -> advance input, RBrace :: tokens
     | '(' -> advance input, LParen :: tokens
     | ')' -> advance input, RParen :: tokens
     | '+' -> advance input, Plus :: tokens
     | '-' -> advance input, Minus :: tokens
     | '*' -> advance input, Times :: tokens
     | '!' -> advance input, Deref :: tokens
     | '%' -> advance input, Modulo :: tokens
     | '^' -> advance input, StrConcat :: tokens
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
     | '"' ->
       let str =
         S.take_while (S.drop_prefix input 1) ~f:(fun c -> not @@ Char.equal c '"')
       in
       (match S.get input (S.length str + 1) with
        | '"' ->
          ( advance_n input (S.length str + 2)
          , StringLiteral (sanitize_string str) :: tokens )
        | _ -> failwith "Invalid string literal")
     | c when Char.is_digit c ->
       (match lex_number input with
        | None, _ -> failwith "Invalid number representation"
        | Some num, rest -> rest, num :: tokens)
     | c when Char.is_alpha c ->
       (match get_iden input with
        | "and", rest -> rest, LogAnd :: tokens
        | "bool", rest -> rest, Bool :: tokens
        | "else", rest -> rest, Else :: tokens
        | "extern", rest -> rest, Extern :: tokens
        | "false", rest -> rest, False :: tokens
        | "float", rest -> rest, Float :: tokens
        | "fun", rest -> rest, Fun :: tokens
        | "if", rest -> rest, If :: tokens
        | "int", rest -> rest, Int :: tokens
        | "let", rest -> rest, Let :: tokens
        | "or", rest -> rest, LogOr :: tokens
        | "print", rest -> rest, Print :: tokens
        | "ref", rest -> rest, Ref :: tokens
        | "return", rest -> rest, Return :: tokens
        | "string", rest -> rest, String :: tokens
        | "true", rest -> rest, True :: tokens
        | "type", rest -> rest, Type :: tokens
        | "xor", rest -> rest, LogXor :: tokens
        | iden, rest -> rest, Identifier iden :: tokens)
     | _ -> failwith @@ Printf.sprintf "Unknown token: '%s'" input)
    |> lex'
;;

let lex input =
  match lex' (input, []) with
  | tokens, "" -> List.rev tokens
  | _ -> failwith "Failed to lex whole input"
;;
