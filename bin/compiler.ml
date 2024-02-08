[@@@ocaml.warning "-27"]

open Core
open Llvm

let the_context = global_context ()
let the_builder = builder the_context
let the_module = create_module the_context "my module"
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create (module String)

(* let double_type = Llvm.double_type the_context *)
let int_type = Llvm.i32_type the_context
let char_type = Llvm.i8_type the_context
let null_value = const_null int_type

(* write IR to file *)
let write_ir modu =
  dump_module modu;
  let ir = Llvm.string_of_llmodule modu in
  Out_channel.write_all "output.ll" ~data:ir;
  ()
;;

let rec compile (ast : Ast.root) : unit =
  let fun_type = function_type int_type [||] in
  let f = define_function "main" fun_type the_module in
  let builder = builder_at_end the_context (entry_block f) in
  compile_block builder ast |> ignore;
  write_ir the_module

and compile_block (builder : llbuilder) (block : Ast.block) : llvalue =
  match block with
  | Ast.Block stmts ->
    List.iter stmts ~f:(fun stmt -> compile_stmt builder stmt |> ignore);
    null_value

and compile_stmt (builder : llbuilder) (stmt : Ast.statement) : llvalue =
  match stmt with
  | Ast.Assign (name, expr) ->
    let value = compile_expr builder expr in
    (* Llvm.set_value_name name value; *)
    Hashtbl.set named_values ~key:name ~data:value;
    value
  | Ast.Print expr ->
    let printf_ty = var_arg_function_type int_type [| pointer_type char_type |] in
    let printf = declare_function "printf" printf_ty the_module in
    let value = compile_expr builder expr in
    let print_int = build_global_stringptr "%d\n" "int_printer" builder in
    build_call printf [| print_int; value |] "" builder
  | Ast.If (expr, block) ->
    let cond_value = compile_expr builder expr in
    let start_bb = insertion_block builder in
    let parent = block_parent start_bb in
    let then_bb = append_block the_context "then" parent in
    position_at_end then_bb builder;
    compile_block builder block |> ignore;
    let cont_bb = append_block the_context "ifcont" parent in
    position_at_end start_bb builder;
    build_cond_br cond_value then_bb cont_bb builder |> ignore;
    position_at_end cont_bb builder;
    null_value
  | Ast.Return expr ->
    let value = compile_expr builder expr in
    build_ret value builder
  | Ast.FunDef (name, args, body) ->
    let args_t = Array.create ~len:(List.length args) int_type in
    let fun_type = function_type int_type args_t in
    let f = define_function name fun_type the_module in
    let builder = builder_at_end the_context (entry_block f) in
    Hashtbl.clear named_values;
    Array.zip_exn (Array.of_list args) (params f)
    |> Array.iter ~f:(fun (arg_name, param) ->
      Hashtbl.set named_values ~key:arg_name ~data:param;
      set_value_name arg_name param);
    compile_block builder body |> ignore;
    null_value

and compile_expr (builder : llbuilder) (expr : Ast.expression) : llvalue =
  match expr with
  | Ast.Add (lhs, rhs) ->
    let lhs_val = compile_term builder lhs in
    let rhs_val = compile_expr builder rhs in
    build_add lhs_val rhs_val "" builder
  | Ast.Sub (lhs, rhs) ->
    let lhs_val = compile_term builder lhs in
    let rhs_val = compile_expr builder rhs in
    build_sub lhs_val rhs_val "" builder
  | Ast.Lt (lhs, rhs) ->
    let lhs_val = compile_term builder lhs in
    let rhs_val = compile_expr builder rhs in
    build_icmp Icmp.Slt lhs_val rhs_val "" builder
  | Ast.Gt (lhs, rhs) ->
    let lhs_val = compile_term builder lhs in
    let rhs_val = compile_expr builder rhs in
    build_icmp Icmp.Sgt lhs_val rhs_val "" builder
  | Ast.Term term -> compile_term builder term

and compile_term (builder : llbuilder) (term : Ast.term) : llvalue =
  match term with
  | Ast.Mul (lhs, rhs) ->
    let lhs_val = compile_factor builder lhs in
    let rhs_val = compile_term builder rhs in
    build_mul lhs_val rhs_val "" builder
  | Ast.Div (lhs, rhs) ->
    let lhs_val = compile_factor builder lhs in
    let rhs_val = compile_term builder rhs in
    Llvm.build_sdiv lhs_val rhs_val "" builder
  | Ast.Factor factor -> compile_factor builder factor

and compile_factor (builder : llbuilder) (factor : Ast.factor) : llvalue =
  match factor with
  | Ast.Group exp -> compile_expr builder exp
  | Ast.Value valu -> compile_value builder valu
  | Ast.FunCall (name, args) ->
    (match Llvm.lookup_function name the_module with
     | Some f ->
       let args = List.map args ~f:(compile_expr builder) |> Array.of_list in
       if equal (params f |> Array.length) (Array.length args)
       then build_call f args "" builder
       else
         failwith @@ Printf.sprintf "incorrect number of arguments in call to '%s'" name
     | None -> failwith @@ Printf.sprintf "unknown function referenced: '%s'" name)

and compile_value (builder : llbuilder) (value : Ast.value) : llvalue =
  match value with
  | Ast.Literal n -> const_int int_type n
  | Ast.Var name ->
    (match Hashtbl.find named_values name with
     | Some v -> v
     | None -> failwith @@ Printf.sprintf "unknown variable name: '%s'" name)
;;
