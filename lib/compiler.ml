[@@@ocaml.warning "-27"]

open Core
open Llvm

(* llvm setup *)
let the_context = global_context ()
let the_builder = builder the_context
let the_module = create_module the_context "my module"

(* symbol table *)
module SymTable = struct
  type table = (string, llvalue) Hashtbl.t
  type t = table list

  let new_frame () : t = [ Hashtbl.create (module String) ]
  let add_frame t : t = Hashtbl.create (module String) :: t
  let drop_frame t : t = List.tl_exn t

  let find_value t name : llvalue option =
    List.find_map t ~f:(fun scope -> Hashtbl.find scope name)
  ;;
end

module Compiler = struct
  type t =
    { builder : llbuilder
    ; sym_table : SymTable.t
    }

  let new_compiler builder = { builder; sym_table = SymTable.new_frame () }
  let add_frame c = { c with sym_table = SymTable.add_frame c.sym_table }
  let drop_frame c = { c with sym_table = SymTable.drop_frame c.sym_table }
  let find_value c name = SymTable.find_value c.sym_table name

  let add_symbol c name value =
    let scope = List.hd_exn c.sym_table in
    Hashtbl.set scope ~key:name ~data:value;
    c
  ;;
end

(* basic types *)
let int_type = Llvm.i32_type the_context
let char_type = Llvm.i8_type the_context
let null_value = const_null int_type
let fun_type = function_type int_type [||]

(* printf *)
let printf_ty = var_arg_function_type int_type [| pointer_type char_type |]
let printf = declare_function "printf" printf_ty the_module
let print_int = ref null_value

(* write IR to file *)
let write_ir modu =
  dump_module modu;
  let ir = Llvm.string_of_llmodule modu in
  Out_channel.write_all "output.ll" ~data:ir;
  ()
;;

let rec compile (ast : Ast.root) : unit =
  let f = define_function "main" fun_type the_module in
  let builder = builder_at_end the_context (entry_block f) in
  let compiler = Compiler.new_compiler builder in
  print_int := build_global_stringptr "%d\n" "int_printer" builder;
  compile_block compiler ast |> ignore;
  write_ir the_module

and compile_block (c : Compiler.t) (block : Ast.block) : Compiler.t =
  match block with
  | Ast.Block stmts -> List.fold stmts ~init:c ~f:(fun c stmt -> compile_stmt c stmt)

and compile_if (c : Compiler.t) if_blocks maybe_else =
  let c = Compiler.add_frame c in
  let start_bb = insertion_block c.builder in
  let parent = block_parent start_bb in
  let end_bb = append_block the_context "endif" parent in
  position_at_end start_bb c.builder;
  compile_if' c if_blocks maybe_else end_bb |> ignore;
  position_at_end end_bb c.builder;
  Compiler.drop_frame c

and compile_if' (c : Compiler.t) if_blocks maybe_else end_bb =
  let start_bb = insertion_block c.builder in
  let parent = block_parent start_bb in
  match if_blocks, maybe_else with
  | (e, b) :: rest, _ ->
    Printf.printf "len of rest: %d\n" (List.length rest);
    let then_bb = append_block the_context "then" parent in
    let else_bb = append_block the_context "else" parent in
    let cond_value = compile_expr c e in
    build_cond_br cond_value then_bb else_bb c.builder |> ignore;
    position_at_end then_bb c.builder;
    compile_block c b |> ignore;
    build_br end_bb c.builder |> ignore;
    position_at_end else_bb c.builder;
    compile_if' c rest maybe_else end_bb
  | [], Some b ->
    compile_block c b |> ignore;
    build_br end_bb c.builder |> ignore;
    null_value
  | [], None ->
    build_br end_bb c.builder |> ignore;
    null_value

and compile_stmt (c : Compiler.t) (stmt : Ast.statement) : Compiler.t =
  match stmt with
  | Ast.Assign (name, expr) ->
    let value = compile_expr c expr in
    Compiler.add_symbol c name value
  | Ast.Print expr ->
    let value = compile_expr c expr in
    build_call printf [| !print_int; value |] "" c.builder |> ignore;
    c
  | Ast.If (if_blocks, maybe_else) -> compile_if c if_blocks maybe_else
  | Ast.Return expr ->
    let value = compile_expr c expr in
    build_ret value c.builder |> ignore;
    c
  | Ast.FunDef (name, args, body) ->
    let args_t = Array.create ~len:(List.length args) int_type in
    let fun_type = function_type int_type args_t in
    let f = define_function name fun_type the_module in
    let builder = builder_at_end the_context (entry_block f) in
    let fun_c =
      Array.zip_exn (Array.of_list args) (params f)
      |> Array.fold
           ~init:(Compiler.new_compiler builder)
           ~f:(fun acc_c (arg_name, param) ->
             set_value_name arg_name param;
             Compiler.add_symbol acc_c arg_name param)
    in
    (* drop function specific compiler *)
    compile_block fun_c body |> ignore;
    c

and compile_expr (c : Compiler.t) (expr : Ast.expression) : llvalue =
  match expr with
  | Ast.Add (lhs, rhs) -> build_add (compile_term c lhs) (compile_expr c rhs) "" c.builder
  | Ast.Sub (lhs, rhs) -> build_sub (compile_term c lhs) (compile_expr c rhs) "" c.builder
  | Ast.Lt (lhs, rhs) ->
    build_icmp Icmp.Slt (compile_term c lhs) (compile_expr c rhs) "" c.builder
  | Ast.Gt (lhs, rhs) ->
    build_icmp Icmp.Sgt (compile_term c lhs) (compile_expr c rhs) "" c.builder
  | Ast.Term term -> compile_term c term

and compile_term (c : Compiler.t) (term : Ast.term) : llvalue =
  match term with
  | Ast.Mul (lhs, rhs) ->
    build_mul (compile_factor c lhs) (compile_term c rhs) "" c.builder
  | Ast.Div (lhs, rhs) ->
    Llvm.build_sdiv (compile_factor c lhs) (compile_term c rhs) "" c.builder
  | Ast.Factor factor -> compile_factor c factor

and compile_funcall (c : Compiler.t) name args : llvalue =
  match Llvm.lookup_function name the_module with
  | Some f ->
    if equal (params f |> Array.length) (List.length args)
    then (
      let args = List.map args ~f:(compile_expr c) |> Array.of_list in
      build_call f args "" c.builder)
    else failwith @@ Printf.sprintf "incorrect number of arguments in call to '%s'" name
  | None -> failwith @@ Printf.sprintf "unknown function referenced: '%s'" name

and compile_factor (c : Compiler.t) (factor : Ast.factor) : llvalue =
  match factor with
  | Ast.Group exp -> compile_expr c exp
  | Ast.Value valu -> compile_value c valu
  | Ast.FunCall (name, args) -> compile_funcall c name args

and compile_value (c : Compiler.t) (value : Ast.value) : llvalue =
  match value with
  | Ast.Literal n -> const_int int_type n
  | Ast.Var name ->
    (match Compiler.find_value c name with
     | Some v -> v
     | None -> failwith @@ Printf.sprintf "unknown variable name: '%s'" name)
  | _ -> failwith "not implemented"
;;
