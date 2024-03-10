open Core
open Llvm

type type' =
  | Int
  | Float
  | Bool
  | String
  | Char (* i8 *)
  | Pointer of type'
  | Struct of string
[@@deriving show, sexp, eq]

let rec type_of_ast_type = function
  | Ast.Int -> Int
  | Ast.Float -> Float
  | Ast.Bool -> Bool
  | Ast.String -> String
  | Ast.Char -> Char
  | Ast.Pointer t -> Pointer (type_of_ast_type t)
  | Ast.Struct name -> Struct name
  | Ast.Tuple _ -> failwith "todo tuple"
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
    ; fun_table : (string, type' * lltype) Hashtbl.t (* maybe use sym_table? *)
    ; type_table : (string, lltype * (string * type') list) Hashtbl.t
    }

  let new_compiler builder =
    { builder
    ; sym_table = SymTable.new_frame ()
    ; fun_table = Hashtbl.create (module String)
    ; type_table = Hashtbl.create (module String)
    }
  ;;

  let new_compiler_from c builder = { c with builder; sym_table = SymTable.new_frame () }
  let add_frame c = { c with sym_table = SymTable.add_frame c.sym_table }
  let drop_frame c = { c with sym_table = SymTable.drop_frame c.sym_table }
  let find_value c name = SymTable.find_value c.sym_table name
  let add_struct c name type' = Hashtbl.set c.type_table ~key:name ~data:(type', [])

  let set_struct_fields c name fields =
    Hashtbl.update c.type_table name ~f:(function
      | Some (t, _) -> t, fields
      | None -> failwith "unknown struct")
  ;;

  let get_struct c name =
    match Hashtbl.find c.type_table name with
    | Some t -> t
    | None -> failwith "unknown struct"
  ;;

  let get_struct_field_idx c name field =
    match
      let open Option.Let_syntax in
      let%bind _, fields = Hashtbl.find c.type_table name in
      let%map idx, _ = List.findi fields ~f:(fun _ (n, _) -> String.equal n field) in
      idx
    with
    | Some idx -> idx
    | None -> failwith "unknown field in struct"
  ;;

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
let ptr_type = pointer_type the_context
let null_value = const_null int_type
let fun_type = function_type int_type [||]
let bool_type = i1_type the_context

let string_type =
  let str_ty = named_struct_type the_context "String" in
  struct_set_body str_ty [| ptr_type; int_type; int_type |] false;
  str_ty
;;

(* type mapping *)
let map_type c = function
  | Int -> int_type
  | Float -> float_type
  | Bool -> bool_type
  | Char -> char_type
  | Pointer _ty -> ptr_type
  | String -> string_type
  | Struct name -> Compiler.get_struct c name |> fst
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
let printf_ty = var_arg_function_type int_type [| ptr_type |]
let printf = declare_function "printf" printf_ty the_module
let print_int = ref null_value
let print_i8 = ref null_value
let print_float = ref null_value
let print_ptr = ref null_value
let void = void_type the_context

let init_printers builder =
  print_int := build_global_stringptr "%d\n" "int_printer" builder;
  print_i8 := build_global_stringptr "'%d'\n" "i8_printer" builder;
  print_float := build_global_stringptr "%f\n" "float_printer" builder;
  print_ptr := build_global_stringptr "&(%p):" "ptr_printer" builder
;;

(* puts *)
let puts_ty = function_type int_type [| ptr_type |]
let puts = declare_function "puts" puts_ty the_module

(* memcpy *)
let memcpy_ty = function_type ptr_type [| ptr_type; ptr_type; int_type |]
let memcpy = declare_function "memcpy" memcpy_ty the_module

(* malloc *)
let malloc_ty = function_type ptr_type [| int_type |]
let malloc = declare_function "malloc" malloc_ty the_module

(* free *)
let free_ty = function_type void [| ptr_type |]
let free = declare_function "free" free_ty the_module

(* let freeval (c : Compiler.t) v = *)
(*   let ptr = build_bitcast v char_ptr_type "" c.builder in *)
(*   build_call free [| ptr |] "" the_builder |> ignore *)
(* ;; *)

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

and compile_print (c : Compiler.t) value v_t =
  match v_t with
  | Int | Bool | Char -> build_call printf_ty printf [| !print_int; value |] "" c.builder
  | Float -> build_call printf_ty printf [| !print_float; value |] "" c.builder
  | String ->
    let char_ptr = build_extractvalue value 0 "" c.builder in
    build_call puts_ty puts [| char_ptr |] "" c.builder
  | Struct struct_name ->
    let str = build_global_stringptr (struct_name ^ "{todo fields}") "" c.builder in
    build_call puts_ty puts [| str |] "" c.builder
  | Pointer inner_ty ->
    build_call printf_ty printf [| !print_ptr; value |] "" c.builder |> ignore;
    let inner = build_load (map_type c inner_ty) value "" c.builder in
    compile_print c inner inner_ty

and compile_stmt (c : Compiler.t) (stmt : Ast.statement) : Compiler.t =
  match stmt with
  | Ast.Declaration (name, expr) ->
    let value, v_t = compile_expr c expr in
    set_value_name ("var." ^ name) value;
    Compiler.add_symbol c name value v_t
  | Ast.Assign (name_expr, expr) ->
    (match name_expr with
     | Ast.(Value (Var name)) ->
       (match Compiler.find_value c name with
        | None -> failwith @@ Printf.sprintf "unknown variable name: '%s'" name
        | Some (_, t) ->
          let value, v_t = compile_expr c expr in
          if equal_type' t v_t
          then (
            set_value_name ("var." ^ name) value;
            Compiler.add_symbol c name value v_t)
          else failwith "type error: cannot assign to variable of different type")
     | _ -> failwith "can't assign to non-variable")
  | Ast.Print expr ->
    let value, v_t = compile_expr c expr in
    compile_print c value v_t |> ignore;
    c
  | Ast.If (if_blocks, maybe_else) -> compile_if c if_blocks maybe_else
  | Ast.Return expr ->
    let llval, _ = compile_expr c expr in
    build_ret llval c.builder |> ignore;
    c
  | Ast.FunDef (name, args, ret_t, body) ->
    let fun_c = get_fundef_compiler c name args ret_t in
    compile_block fun_c body |> ignore;
    c
  | Ast.TypeDef (type_name, members) ->
    let ty = named_struct_type the_context type_name in
    Compiler.add_struct c type_name ty;
    let members = List.map members ~f:(fun (name, t) -> name, type_of_ast_type t) in
    let member_lltypes =
      List.map members ~f:(fun (_, t) -> t |> map_type c) |> Array.of_list
    in
    Compiler.set_struct_fields c type_name members;
    struct_set_body ty member_lltypes false;
    c
  | Ast.EnumDef _ -> failwith "todo enumdef"

and get_fundef_compiler c name args ret_t =
  let ret_t = type_of_ast_type ret_t in
  let args_t =
    List.map args ~f:(fun (_, t) ->
      let t = type_of_ast_type t in
      map_type c t)
    |> Array.of_list
  in
  let fun_type = function_type (map_type c ret_t) args_t in
  let f = define_function name fun_type the_module in
  let builder = builder_at_end the_context (entry_block f) in
  Compiler.add_fun_t c name (ret_t, fun_type);
  Array.zip_exn (Array.of_list args) (params f)
  |> Array.fold
       ~init:(Compiler.new_compiler_from c builder)
       ~f:(fun acc_c ((arg_name, arg_t), param) ->
         set_value_name ("arg." ^ arg_name) param;
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
  let concat_fn = lookup_function "builtin.concat.string" the_module in
  let concat_fn = Option.value_exn concat_fn in
  match Compiler.get_fun_t c "builtin.concat.string" with
  | Some (_ret_t, fun_t) ->
    build_call fun_t concat_fn [| lhs_val; rhs_val |] "str_concat" c.builder, String
  | None -> failwith "asd"

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
  | Ast.Access (expr, member) ->
    (match compile_expr c expr with
     | llval, Struct struct_name ->
       let idx = Compiler.get_struct_field_idx c struct_name member in
       let field_val = build_extractvalue llval idx ("struct_name." ^ member) c.builder in
       let _, members = Compiler.get_struct c struct_name in
       field_val, snd @@ List.nth_exn members idx
     | _ -> failwith "type error: cannot access member of non-struct type")
  | Ast.(UnOp (Ref, e)) ->
    let fv, ft = compile_expr c e in
    let malloc = build_malloc (map_type c ft) "" c.builder in
    (match ft with
     | Struct _ -> build_store fv malloc c.builder |> ignore
     | _ -> build_store fv malloc c.builder |> ignore);
    malloc, Pointer ft
  | Ast.(UnOp (Deref, e)) ->
    (match compile_expr c e with
     | v, Pointer t -> build_load (map_type c t) v "" c.builder, t
     | _ -> failwith "type error: cannot dereference non-pointer type")
  | Ast.FunCall (name, args) ->
    let v, t = compile_funcall c name args in
    set_value_name (name ^ ".res") v;
    v, t
  | Ast.Value valu -> compile_value c valu

and compile_funcall (c : Compiler.t) name args : llvalue * type' =
  match lookup_function name the_module, Compiler.get_fun_t c name with
  | Some f, Some (ret_t, f_t) ->
    if equal (params f |> Array.length) (List.length args)
    then (
      let args =
        List.map args ~f:(fun e ->
          let v, _t = compile_expr c e in
          v)
        |> Array.of_list
      in
      build_call f_t f args "" c.builder, ret_t)
    else failwith @@ Printf.sprintf "incorrect number of arguments in call to '%s'" name
  | _ -> failwith @@ Printf.sprintf "unknown function referenced: '%s'" name

and compile_value (c : Compiler.t) (value : Ast.value) : llvalue * type' =
  match value with
  | Ast.ILiteral n -> const_int int_type n, Int
  | Ast.FPLiteral n -> const_float float_type n, Float
  | Ast.BoolLiteral b -> const_int bool_type (if b then 1 else 0), Bool
  | Ast.Var name ->
    (match Compiler.find_value c name with
     | Some v -> v
     | None -> failwith @@ Printf.sprintf "unknown variable name: '%s'" name)
  | Ast.StringLiteral s ->
    let strptr = Llvm.build_global_stringptr s "strptr" c.builder in
    let lllen = const_int int_type (String.length s) in
    let create_fn =
      Option.value_exn @@ Llvm.lookup_function "builtin.create.string.literal" the_module
    in
    let _, llty =
      Option.value_exn @@ Compiler.get_fun_t c "builtin.create.string.literal"
    in
    build_call llty create_fn [| strptr; lllen |] "strlit" c.builder, String
  | Ast.StructLiteral (type_name, members) ->
    let ty, _ = Compiler.get_struct c type_name in
    let stack_ptr = build_alloca ty "" c.builder in
    List.iter members ~f:(fun (field, value) ->
      let value, _t = compile_expr c value in
      let idx = Compiler.get_struct_field_idx c type_name field in
      let field_ptr = build_struct_gep ty stack_ptr idx "" c.builder in
      build_store value field_ptr c.builder |> ignore);
    let reg_struct = build_load ty stack_ptr "" c.builder in
    reg_struct, Struct type_name
  | Ast.TupleLiteral _ -> failwith "todo tuple literal"

(* Built-in functions *)
and init_builtin_functions c =
  builtin_create_string_literal c;
  builtin_concat_strings c;
  Compiler.add_fun_t c "puts" (Int, puts_ty)

and builtin_create_string_literal c =
  let c =
    get_fundef_compiler
      c
      "builtin.create.string.literal"
      [ "str_ptr", Ast.Pointer Ast.Char; "len", Ast.Int ]
      Ast.String
  in
  let strptr, _ = Option.value_exn @@ Compiler.find_value c "str_ptr" in
  let lllen, _ = Option.value_exn @@ Compiler.find_value c "len" in
  let const_str = build_insertvalue (const_null string_type) strptr 0 "" c.builder in
  let const_str = build_insertvalue const_str lllen 1 "" c.builder in
  let const_str = build_insertvalue const_str lllen 2 "" c.builder in
  build_ret const_str c.builder |> ignore

and builtin_concat_strings c =
  let c =
    get_fundef_compiler
      c
      "builtin.concat.string"
      [ "lhs", Ast.String; "rhs", Ast.String ]
      Ast.String
  in
  let lhs, _ = Option.value_exn @@ Compiler.find_value c "lhs" in
  let rhs, _ = Option.value_exn @@ Compiler.find_value c "rhs" in
  let len_lhs = build_extractvalue lhs 1 "" c.builder in
  let len_rhs = build_extractvalue rhs 1 "" c.builder in
  let total_len = build_add len_lhs len_rhs "tot_len" c.builder in
  let heap = build_array_malloc char_type total_len ".string.heap_array" c.builder in
  let lhs_ptr = build_extractvalue lhs 0 "" c.builder in
  build_call memcpy_ty memcpy [| heap; lhs_ptr; len_lhs |] "" c.builder |> ignore;
  let rhs_ptr = build_extractvalue rhs 0 "" c.builder in
  let rhs_start = build_gep char_type heap [| len_lhs |] "rhs_start" c.builder in
  build_call memcpy_ty memcpy [| rhs_start; rhs_ptr; len_rhs |] "" c.builder |> ignore;
  let ret = build_insertvalue (const_null string_type) heap 0 "" c.builder in
  let ret = build_insertvalue ret total_len 1 "" c.builder in
  let ret = build_insertvalue ret total_len 2 "" c.builder in
  build_ret ret c.builder |> ignore
;;
