open Core
open Llvm

type type' =
  | Int
  | Float
  | Bool
  | String
  | Char (* i8 *)
  | Pointer of type'
[@@deriving show, sexp, eq]

let rec type_of_ast_type = function
  | Ast.Int -> Int
  | Ast.Float -> Float
  | Ast.Bool -> Bool
  | Ast.String -> String
  | Ast.Char -> Char
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
  | Char -> char_type
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
  | Concat

(* printf *)
let printf_ty = var_arg_function_type int_type [| pointer_type char_type |]
let printf = declare_function "printf" printf_ty the_module
let print_int = ref null_value
let print_i8 = ref null_value
let print_float = ref null_value
let print_ptr = ref null_value
let void = void_type the_context
let void_ptr = pointer_type void

let init_printers builder =
  print_int := build_global_stringptr "%d\n" "int_printer" builder;
  print_i8 := build_global_stringptr "'%d'\n" "i8_printer" builder;
  print_float := build_global_stringptr "%f\n" "float_printer" builder;
  print_ptr := build_global_stringptr "&(%p)\n" "ptr_printer" builder
;;

(* puts *)
let puts_ty = function_type int_type [| pointer_type char_type |]
let puts = declare_function "puts" puts_ty the_module

(* memcpy *)
let memcpy_ty = function_type char_ptr_type [| char_ptr_type; char_ptr_type; int_type |]
let memcpy = declare_function "memcpy" memcpy_ty the_module

(* malloc *)
let malloc_ty = function_type char_ptr_type [| int_type |]
let malloc = declare_function "malloc" malloc_ty the_module

(* free *)
(* let free_ty = function_type void [| char_ptr_type |] *)
(* let free = declare_function "free" free_ty the_module *)

(* write IR to file *)
let write_ir modu =
  let ir = Llvm.string_of_llmodule modu in
  Out_channel.write_all "output.ll" ~data:ir;
  ()
;;

(* LLVM IR generation *)
let rec compile (ast : Ast.root) : unit =
  let f = define_function "main" fun_type the_module in
  let builder = builder_at_end the_context (entry_block f) in
  let compiler = Compiler.new_compiler builder in
  init_printers builder;
  init_builtin_functions compiler;
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
    set_value_name ("var." ^ name) value;
    Compiler.add_symbol c name value v_t
  | Ast.Print expr ->
    let value, v_t = compile_expr c expr in
    (match v_t with
     | Int | Bool | Char -> build_call printf [| !print_int; value |] "" c.builder
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
  | Ast.FunDef (name, args, ret_t, body) ->
    compile_block (get_fundef_compiler c name args ret_t) body |> ignore;
    c

and get_fundef_compiler c name args ret_t =
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
  Array.zip_exn (Array.of_list args) (params f)
  |> Array.fold
       ~init:(Compiler.new_compiler_from c builder)
       ~f:(fun acc_c ((arg_name, arg_t), param) ->
         set_value_name arg_name param;
         Compiler.add_symbol acc_c arg_name param (type_of_ast_type arg_t))

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
  | Concat -> String

and compile_string_concat (c : Compiler.t) lhs_val rhs_val =
  let concat_fn = Llvm.lookup_function "builtin.concat.string" the_module in
  let concat_fn = Option.value_exn concat_fn in
  build_call concat_fn [| lhs_val; rhs_val |] "str_concat" c.builder, String

and compile_binop
  (c : Compiler.t)
  ((lhs, l_type) : llvalue * type')
  (op : binop)
  ((rhs, r_type) : llvalue * type')
  =
  match l_type, r_type with
  | Int, Int ->
    (get_num_instruction op Int) lhs rhs "int.binop" c.builder, return_type op Int
  | Float, Float ->
    (get_num_instruction op Float) lhs rhs "fp.binop" c.builder, return_type op Float
  | Bool, Bool -> (get_bool_instruction op) lhs rhs "bool.binop" c.builder, Bool
  | String, String ->
    (match op with
     | Concat -> compile_string_concat c lhs rhs
     | _ -> failwith "unimplemented string operation")
  | _ -> failwith "type error: invalid binop types"

and binop_expr_to_op = function
  | Ast.Add -> Add
  | Ast.Eq -> Eq
  | Ast.Gt -> Gt
  | Ast.Lt -> Lt
  | Ast.Sub -> Sub
  | Ast.Mul -> Mul
  | Ast.Div -> Div
  | Ast.Modulo -> Rem
  | Ast.LogOr -> LogOr
  | Ast.LogAnd -> LogAnd
  | Ast.LogXor -> LogXor
  | Ast.Concat -> Concat

and compile_expr (c : Compiler.t) (expr : Ast.expression) : llvalue * type' =
  match expr with
  | Ast.(BinOp (op, lhs, rhs)) ->
    compile_binop c (compile_expr c lhs) (binop_expr_to_op op) (compile_expr c rhs)
  | Ast.(UnOp (Ref, e)) ->
    let fv, ft = compile_expr c e in
    let ptr_ty = Pointer ft in
    let malloc = build_malloc (map_type ft) "" c.builder in
    build_store fv malloc c.builder |> ignore;
    malloc, ptr_ty
  | Ast.(UnOp (Deref, e)) ->
    (match compile_expr c e with
     | v, Pointer t -> build_load v "" c.builder, t
     | _ -> failwith "type error: cannot dereference non-pointer type")
  | Ast.FunCall (name, args) ->
    let v, t = compile_funcall c name args in
    Llvm.set_value_name (name ^ ".res") v;
    v, t
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
    let strptr = Llvm.build_global_stringptr s "strptr" c.builder in
    let lllen = const_int int_type (String.length s) in
    let create_fn =
      Option.value_exn @@ Llvm.lookup_function "builtin.create.string.literal" the_module
    in
    build_call create_fn [| strptr; lllen |] "strlit" c.builder, String
  | Ast.Var name ->
    (match Compiler.find_value c name with
     | Some v -> v
     | None -> failwith @@ Printf.sprintf "unknown variable name: '%s'" name)
  | Ast.BoolLiteral b -> const_int bool_type (if b then 1 else 0), Bool

(* Built-in functions *)
and init_builtin_functions c =
  builtin_create_string_literal c;
  builtin_concat_strings c

and builtin_create_string_literal c =
  let c =
    get_fundef_compiler
      c
      "builtin.create.string.literal"
      [ "str_ptr", Ast.Pointer Ast.Char; "len", Ast.Int ]
      Ast.String
  in
  (* create string literal code gen *)
  let strptr, _ = Option.value_exn @@ Compiler.find_value c "str_ptr" in
  let lllen, _ = Option.value_exn @@ Compiler.find_value c "len" in
  (* build string struct *)
  let string_stack_ptr = build_alloca string_type ".string" c.builder in
  let arr_ptr = build_struct_gep string_stack_ptr 0 ".string.ptr" c.builder in
  let len_ptr = build_struct_gep string_stack_ptr 1 ".string.len" c.builder in
  let cap_ptr = build_struct_gep string_stack_ptr 2 ".string.cap" c.builder in
  (* store stuff in stack struct *)
  build_store strptr arr_ptr c.builder |> ignore;
  build_store lllen len_ptr c.builder |> ignore;
  build_store lllen cap_ptr c.builder |> ignore;
  (* load struct from stack to return *)
  let ret = build_load string_stack_ptr ".string.struct" c.builder in
  build_ret ret c.builder |> ignore

and builtin_concat_strings c =
  let c =
    get_fundef_compiler
      c
      "builtin.concat.string"
      [ "lhs", Ast.String; "rhs", Ast.String ]
      Ast.String
  in
  (* concat strings code gen *)
  let lhs_val, _ = Option.value_exn @@ Compiler.find_value c "lhs" in
  let rhs_val, _ = Option.value_exn @@ Compiler.find_value c "rhs" in
  (* store lhs and rhs in stack *)
  let lhs = build_alloca string_type "" c.builder in
  let rhs = build_alloca string_type "" c.builder in
  build_store lhs_val lhs c.builder |> ignore;
  build_store rhs_val rhs c.builder |> ignore;
  (* calculate length of new string and allocate space in heap *)
  let len_lhs' = build_struct_gep lhs 1 "" c.builder in
  let len_lhs = build_load len_lhs' "len_lhs" c.builder in
  let len_rhs' = build_struct_gep rhs 1 "" c.builder in
  let len_rhs = build_load len_rhs' "len_rhs" c.builder in
  let total_len = build_add len_lhs len_rhs "tot_len" c.builder in
  let heap = build_array_malloc char_type total_len ".string.heap_array" c.builder in
  (* copy lhs to heap *)
  let lhs_ptr' = build_struct_gep lhs 0 "" c.builder in
  let lhs_ptr = build_load lhs_ptr' "lhs_ptr" c.builder in
  build_call memcpy [| heap; lhs_ptr; len_lhs |] "" c.builder |> ignore;
  (* copy rhs to heap *)
  let rhs_ptr' = build_struct_gep rhs 0 "" c.builder in
  let rhs_ptr = build_load rhs_ptr' "rhs_ptr" c.builder in
  let rhs_start = build_gep heap [| len_lhs |] "rhs_start" c.builder in
  build_call memcpy [| rhs_start; rhs_ptr; len_rhs |] "" c.builder |> ignore;
  (* build string struct (stack) *)
  let string_stack_ptr = build_alloca string_type ".string" c.builder in
  let arr_ptr = build_struct_gep string_stack_ptr 0 ".string.ptr" c.builder in
  let len_ptr = build_struct_gep string_stack_ptr 1 ".string.len" c.builder in
  let cap_ptr = build_struct_gep string_stack_ptr 2 ".string.cap" c.builder in
  (* store stuff in stack struct *)
  build_store heap arr_ptr c.builder |> ignore;
  build_store total_len len_ptr c.builder |> ignore;
  build_store total_len cap_ptr c.builder |> ignore;
  (* load struct from stack to return *)
  let ret = build_load string_stack_ptr ".string.struct" c.builder in
  build_ret ret c.builder |> ignore
;;
