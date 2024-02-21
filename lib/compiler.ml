open Core
open Llvm

type type' =
  | Int
  | Float
  | Bool
  | String
  | Pointer of type'
[@@deriving show, sexp, eq]

let rec type_of_ast_type = function
  | Ast.Int -> Int
  | Ast.Float -> Float
  | Ast.Bool -> Bool
  | Ast.String -> String
  | Ast.Pointer t -> Pointer (type_of_ast_type t)
;;

(* symbol table *)
module SymTable = struct
  type table = (string, llvalue * type') Hashtbl.t
  type t = table list

  let new_frame () : t = [ Hashtbl.create (module String) ]
  let add_frame t : t = Hashtbl.create (module String) :: t
  let drop_frame t : t = List.tl_exn t

  let find_value t name : (llvalue * type') option =
    List.find_map t ~f:(fun scope -> Hashtbl.find scope name)
  ;;
end

module Compiler = struct
  type t =
    { builder : llbuilder
    ; sym_table : SymTable.t
    ; fun_table : (string, type') Hashtbl.t (* maybe use sym_table? *)
    }

  let new_compiler builder =
    { builder
    ; sym_table = SymTable.new_frame ()
    ; fun_table = Hashtbl.create (module String)
    }
  ;;

  let new_compiler_from c builder = { c with builder; sym_table = SymTable.new_frame () }
  let add_frame c = { c with sym_table = SymTable.add_frame c.sym_table }
  let drop_frame c = { c with sym_table = SymTable.drop_frame c.sym_table }
  let find_value c name = SymTable.find_value c.sym_table name

  let add_symbol c name value typ =
    let scope = List.hd_exn c.sym_table in
    Hashtbl.set scope ~key:name ~data:(value, typ);
    c
  ;;

  let add_fun_t c name t = Hashtbl.set c.fun_table ~key:name ~data:t
  let get_fun_t c name = Hashtbl.find c.fun_table name
end

(* llvm setup *)
let the_context = global_context ()
let the_builder = builder the_context
let the_module = create_module the_context "my module"

(* basic types *)
let int_type = i32_type the_context
let float_type = double_type the_context
let char_type = i8_type the_context
let char_ptr_type = pointer_type char_type
let null_value = const_null int_type
let fun_type = function_type int_type [||]
let bool_type = i1_type the_context

let string_type =
  let str_ty = named_struct_type the_context "String" in
  Llvm.struct_set_body str_ty [| pointer_type char_type; int_type; int_type |] false;
  str_ty
;;

(* type mapping *)
let rec map_type = function
  | Int -> int_type
  | Float -> float_type
  | Bool -> bool_type
  | Pointer ty -> pointer_type (map_type ty)
  | String -> string_type
;;

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Eq
  | Lt
  | Gt
  | LogOr
  | LogAnd
  | LogXor

(* printf *)
let printf_ty = var_arg_function_type int_type [| pointer_type char_type |]
let printf = declare_function "printf" printf_ty the_module
let print_int = ref null_value
let print_i8 = ref null_value
let print_float = ref null_value
let print_ptr = ref null_value
let void = void_type the_context
let void_ptr = pointer_type void

(* puts *)
let puts_ty = function_type int_type [| pointer_type char_type |]
let puts = declare_function "puts" puts_ty the_module

(* memcpy *)
let memcpy_ty = function_type char_ptr_type [| char_ptr_type; char_ptr_type; int_type |]
let memcpy = declare_function "memcpy" memcpy_ty the_module

let init_printers builder =
  print_int := build_global_stringptr "%d\n" "int_printer" builder;
  print_i8 := build_global_stringptr "'%d'\n" "i8_printer" builder;
  print_float := build_global_stringptr "%f\n" "float_printer" builder;
  print_ptr := build_global_stringptr "&(%p)\n" "ptr_printer" builder
;;

(* write IR to file *)
let write_ir modu =
  (* dump_module modu; *)
  let ir = Llvm.string_of_llmodule modu in
  Out_channel.write_all "output.ll" ~data:ir;
  ()
;;

let rec compile (ast : Ast.root) : unit =
  let f = define_function "main" fun_type the_module in
  let builder = builder_at_end the_context (entry_block f) in
  let compiler = Compiler.new_compiler builder in
  init_printers builder;
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
    let then_bb = append_block the_context "then" parent in
    let else_bb = append_block the_context "else" parent in
    let cond_value, _cond_t = compile_expr c e in
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
    let value, v_t = compile_expr c expr in
    Compiler.add_symbol c name value v_t
  | Ast.Print expr ->
    let value, v_t = compile_expr c expr in
    (match v_t with
     | Int | Bool -> build_call printf [| !print_int; value |] "" c.builder
     | Float -> build_call printf [| !print_float; value |] "" c.builder
     | String ->
       let stack_ptr = build_alloca string_type "" c.builder in
       build_store value stack_ptr c.builder |> ignore;
       let char_ptr_ptr = build_struct_gep stack_ptr 0 "" c.builder in
       let char_ptr = build_load char_ptr_ptr "" c.builder in
       let first_char = build_gep char_ptr [| const_int int_type 0 |] "" c.builder in
       build_call puts [| first_char |] "" c.builder
     | Pointer _ ->
       Printf.printf "pointer type: %s\n" (Sexp.to_string_hum (sexp_of_type' v_t));
       build_call printf [| !print_ptr; value |] "" c.builder)
    |> ignore;
    c
  | Ast.If (if_blocks, maybe_else) -> compile_if c if_blocks maybe_else
  | Ast.Return expr ->
    let value, _v_t = compile_expr c expr in
    build_ret value c.builder |> ignore;
    c
  | Ast.FunDef (name, args, body, ret_t) ->
    let ret_t = type_of_ast_type ret_t in
    let args_t =
      List.map args ~f:(fun (_, t) ->
        let t = type_of_ast_type t in
        map_type t)
      |> Array.of_list
    in
    let fun_type = function_type (map_type ret_t) args_t in
    let f = define_function name fun_type the_module in
    let builder = builder_at_end the_context (entry_block f) in
    Compiler.add_fun_t c name ret_t;
    let fun_c =
      Array.zip_exn (Array.of_list args) (params f)
      |> Array.fold
           ~init:(Compiler.new_compiler_from c builder)
           ~f:(fun acc_c ((arg_name, arg_t), param) ->
             set_value_name arg_name param;
             Compiler.add_symbol acc_c arg_name param (type_of_ast_type arg_t))
    in
    (* drop function specific compiler *)
    compile_block fun_c body |> ignore;
    c

and get_num_instruction op typ =
  match op, typ with
  | Add, Float -> build_fadd
  | Add, Int -> build_add
  | Div, Float -> build_fdiv
  | Div, Int -> build_sdiv
  | Gt, Float -> build_fcmp Fcmp.Ogt
  | Gt, Int -> build_icmp Icmp.Sgt
  | Lt, Float -> build_fcmp Fcmp.Olt
  | Lt, Int -> build_icmp Icmp.Slt
  | Eq, Int -> build_icmp Icmp.Eq
  | Eq, Float -> build_fcmp Fcmp.Oeq
  | Mul, Float -> build_fmul
  | Mul, Int -> build_mul
  | Rem, Float -> build_frem
  | Rem, Int -> build_srem
  | Sub, Float -> build_fsub
  | Sub, Int -> build_sub
  | _ -> failwith "type error: no numeric instruction for binop types"

and get_bool_instruction op =
  match op with
  | LogOr -> build_or
  | LogAnd -> build_and
  | LogXor -> build_xor
  | Eq -> build_icmp Icmp.Eq
  | _ -> failwith "type error: no boolean instruction for binop types"

and return_type op typ =
  match op with
  | Eq | Lt | Gt -> Bool
  | Add | Sub | Mul | Div | Rem -> typ
  | LogOr | LogAnd | LogXor -> Bool

and compile_binop
  (c : Compiler.t)
  ((lhs, l_type) : llvalue * type')
  (op : binop)
  ((rhs, r_type) : llvalue * type')
  =
  match l_type, r_type with
  | Int, Int -> (get_num_instruction op Int) lhs rhs "" c.builder, return_type op Int
  | Float, Float ->
    (get_num_instruction op Float) lhs rhs "" c.builder, return_type op Float
  | Bool, Bool -> (get_bool_instruction op) lhs rhs "" c.builder, Bool
  | _ -> failwith "type error: invalid binop types"

and binop_expr_to_op = function
  | Ast.(BinOp (Add, _, _)) -> Add
  | Ast.(BinOp (Eq, _, _)) -> Eq
  | Ast.(BinOp (Gt, _, _)) -> Gt
  | Ast.(BinOp (Lt, _, _)) -> Lt
  | Ast.(BinOp (Sub, _, _)) -> Sub
  | Ast.(BinOp (Mul, _, _)) -> Mul
  | Ast.(BinOp (Div, _, _)) -> Div
  | Ast.(BinOp (Modulo, _, _)) -> Rem
  | Ast.(BinOp (LogOr, _, _)) -> LogOr
  | Ast.(BinOp (LogAnd, _, _)) -> LogAnd
  | Ast.(BinOp (LogXor, _, _)) -> LogXor
  | _ -> failwith "invalid binop expr"

and compile_expr (c : Compiler.t) (expr : Ast.expression) : llvalue * type' =
  match expr with
  | Ast.(BinOp (_, lhs, rhs)) ->
    compile_binop c (compile_expr c lhs) (binop_expr_to_op expr) (compile_expr c rhs)
  | Ast.(UnOp (Ref, e)) ->
    let fv, ft = compile_expr c e in
    let ptr_ty = Pointer ft in
    let malloc, ptr_ty = build_malloc (map_type ft) "" c.builder, ptr_ty in
    build_store fv malloc c.builder |> ignore;
    malloc, ptr_ty
  | Ast.(UnOp (Deref, e)) ->
    (match compile_expr c e with
     | v, Pointer t -> build_load v "" c.builder, t
     | _ -> failwith "type error: cannot dereference non-pointer type")
  | Ast.FunCall (name, args) -> compile_funcall c name args
  | Ast.Value valu -> compile_value c valu

and compile_funcall (c : Compiler.t) name args : llvalue * type' =
  match Llvm.lookup_function name the_module, Compiler.get_fun_t c name with
  | Some f, Some t ->
    if equal (params f |> Array.length) (List.length args)
    then (
      let args =
        List.map args ~f:(fun e ->
          let v, _t = compile_expr c e in
          v)
        |> Array.of_list
      in
      build_call f args "" c.builder, t)
    else failwith @@ Printf.sprintf "incorrect number of arguments in call to '%s'" name
  | _ -> failwith @@ Printf.sprintf "unknown function referenced: '%s'" name

and compile_value (c : Compiler.t) (value : Ast.value) : llvalue * type' =
  match value with
  | Ast.ILiteral n -> const_int int_type n, Int
  | Ast.FPLiteral n -> const_float float_type n, Float
  | Ast.StringLiteral s ->
    let len = String.length s in
    let lllen = const_int int_type len in
    let string_stack_ptr = build_alloca string_type "" c.builder in
    let byte_arr_heap_ptr = build_array_malloc char_type lllen "" c.builder in
    let arr_ptr = build_struct_gep string_stack_ptr 0 "" c.builder in
    let str_val =
      let p = build_alloca (array_type char_type len) "" c.builder in
      build_store (const_string the_context s) p c.builder |> ignore;
      build_bitcast p char_ptr_type "" c.builder
    in
    let str_ptr = build_gep str_val [| const_int int_type 0 |] "" c.builder in
    build_store byte_arr_heap_ptr arr_ptr c.builder |> ignore;
    build_call memcpy [| byte_arr_heap_ptr; str_ptr; lllen |] "" c.builder |> ignore;
    let len_ptr = build_struct_gep string_stack_ptr 1 "" c.builder in
    build_store lllen len_ptr c.builder |> ignore;
    let cap_ptr = build_struct_gep string_stack_ptr 2 "" c.builder in
    build_store lllen cap_ptr c.builder |> ignore;
    build_load string_stack_ptr "" c.builder, String
  | Ast.Var name ->
    (match Compiler.find_value c name with
     | Some v -> v
     | None -> failwith @@ Printf.sprintf "unknown variable name: '%s'" name)
  | Ast.BoolLiteral b -> const_int bool_type (if b then 1 else 0), Bool
;;
