(* open Alcotest *)
open Tick

(* let test_hello_with_name name () = *)
(*   let greeting = "Hello " ^ name ^ "!" in *)
(*   let expected = Printf.sprintf "Hello %s!" name in *)
(*   check string "same string" greeting expected *)
(* ;; *)

let test_value_parse_number () =
  let ts = [ Token.Number 42 ] in
  let expected = Ast.Literal 42 in
  match Parser.value ts with
  | Some (v, []) -> Alcotest.(check bool) "equals" Ast.(equal_value expected v) true
  | _ -> Alcotest.fail "fail"
;;

let test_value_ident () =
  let ts = [ Token.Identifier "foo" ] in
  let expected = Ast.Var "foo" in
  match Parser.value ts with
  | Some (v, []) -> Alcotest.(check bool) "equals" Ast.(equal_value expected v) true
  | _ -> Alcotest.fail "fail"
;;

let value_suite =
  [ "number literal", `Quick, test_value_parse_number
  ; "identifier", `Quick, test_value_ident
  ]
;;

(* and factor = *)
(*   | Group of expression *)
(*   | FunCall of string * expression list *)
(*   | Value of value *)
(* [@@deriving show, sexp, eq] *)

let test_factor ~tokens ~expect () =
  match Parser.factor tokens with
  | Some (f, []) -> Alcotest.(check bool) "equals" Ast.(equal_factor expect f) true
  | _ -> Alcotest.fail "fail"
;;

let factor_suite =
  [ ( "value"
    , `Quick
    , test_factor ~tokens:[ Token.Identifier "foo" ] ~expect:Ast.(Value (Var "foo")) )
  ; ( "group"
    , `Quick
    , test_factor
        ~tokens:[ Token.LParen; Token.Number 2; Token.RParen ]
        ~expect:Ast.(Group (Term (Factor (Value (Literal 2))))) )
  ; ( "fun call"
    , `Quick
    , test_factor
        ~tokens:[ Token.Identifier "foo"; Token.LParen; Token.Number 2; Token.RParen ]
        ~expect:Ast.(FunCall ("foo", [ Term (Factor (Value (Literal 2))) ])) )
  ]
;;

let () = Alcotest.run "Parser" [ "Value", value_suite; "Factor", factor_suite ]
