open Core
open Tick

let pp tokens =
  List.iter tokens ~f:(fun token -> print_endline @@ Token.show token);
  tokens
;;

let compile source =
  match source |> Lexer.lex |> pp |> Parser.parse with
  | None -> print_endline "Failed to parse"
  | Some (ast, tokens) ->
    print_endline "\nAST:";
    Ast.sexp_of_root ast |> Sexp.to_string_hum |> print_endline;
    (match tokens with
     | [ Token.Eof ] -> ()
     | _ ->
       print_endline "\nUNPARSED TOKENS:";
       pp tokens |> ignore);
    print_endline "\nIR:";
    Compiler.compile ast
;;

let () =
  let argv = Sys.get_argv () in
  if Array.length argv <> 2
  then failwith "Usage: ./main <input file>"
  else In_channel.read_all argv.(1) |> compile
;;
