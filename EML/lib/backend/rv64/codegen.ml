(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Riscv
open Middleend.Anf_tree
open Frontend.Ast

exception Compile_error of string

module S = Set.Make (String)
module StringSet = Set.Make (String)

let lambda_counter = ref 0

let fresh_label prefix =
  let c = ref 0 in
  fun () ->
    incr c;
    prefix ^ string_of_int !c
;;

let fresh_else = fresh_label "else"
let fresh_end = fresh_label "end"
let fresh_lambda = fresh_label "lambda"
let word_size = 8
let tag_int n = n lsl 1

type env =
  { locals : (ident * int) list
  ; params : (ident * int) list
  ; next_local_offset : int
  ; locals_base : int
  ; max_local_offset : int
  ; globals : StringSet.t
  ; is_compiling_global : bool
  ; self_var : ident option
  }

let empty_env =
  { locals = []
  ; params = []
  ; next_local_offset = 0
  ; locals_base = 0
  ; max_local_offset = 0
  ; globals = StringSet.empty
  ; is_compiling_global = false
  ; self_var = None
  }
;;

let bump_stack env bytes =
  let next = env.next_local_offset + bytes in
  { env with next_local_offset = next; max_local_offset = max env.max_local_offset next }
;;

type location =
  | Stack of int
  | Reg of reg
  | GlobalFunc of ident

let lookup x env =
  match List.assoc_opt x env.locals with
  | Some off -> Stack off
  | None ->
    (match List.assoc_opt x env.params with
     | Some off -> Stack off
     | None ->
       if StringSet.mem x env.globals
       then GlobalFunc x
       else raise (Compile_error (Printf.sprintf "Unbound variable: %s" x)))
;;

type cg_state =
  { mutable functions : instr list
  ; mutable data : data list
  ; mutable globals : StringSet.t
  ; mutable main_code : instr list
  }

let gen_immediate env = function
  | ImmediateConst (ConstInt n) -> [ Li (result_reg, tag_int n) ]
  | ImmediateConst (ConstBool b) -> [ Li (result_reg, if b then 3 else 1) ]
  | ImmediateConst (ConstString _) ->
    raise (Compile_error "String constants are not supported yet")
  | ImmediateVar x ->
    (match lookup x env with
     | Stack off -> [ Ld (result_reg, off, Fp) ]
     | Reg r -> [ Addi (result_reg, r, 0) ]
     | GlobalFunc fname -> [ La (result_reg, fname) ])
;;

let gen_binop op =
  match op with
  | Plus -> [ Add (temp_reg, temp_reg2, temp_reg3) ]
  | Minus -> [ Sub (temp_reg, temp_reg2, temp_reg3) ]
  | Multiply -> [ Mul (temp_reg, temp_reg2, temp_reg3) ]
  | LowerThan -> [ Slt (temp_reg, temp_reg2, temp_reg3) ]
  | LowestEqual -> [ Slt (temp_reg, temp_reg2, temp_reg3); Xori (temp_reg, temp_reg, 1) ]
  | Division | And | Or | GretestEqual | GreaterThan | Equal | NotEqual ->
    raise (Compile_error (Printf.sprintf "Binary operator not implemented"))
;;

let rec gen_complex env = function
  | ComplexImmediate i -> gen_immediate env i, env
  | ComplexLambda _ ->
    (match env.self_var with
     | Some name -> [ La (result_reg, name) ], env
     | None -> raise (Compile_error "Global lambda must have a self_var"))
  | ComplexApp (f, arg, []) ->
    (match f with
     | ImmediateVar fname when StringSet.mem fname env.globals ->
       ( gen_immediate env arg @ [ Mv (A 0, result_reg); Call fname; Mv (result_reg, A 0) ]
       , env )
     | _ ->
       ( gen_immediate env f
         @ gen_immediate env arg
         @ [ Mv (A 0, result_reg)
           ; Ld (T 0, 0, result_reg)
           ; Jalr (Ra, T 0, 0)
           ; Mv (result_reg, A 0)
           ]
       , env ))
  | ComplexBinOper (op, a, b) ->
    let code_a = gen_immediate env a in
    let code_b = gen_immediate env b in
    (match op with
     | LowerThan ->
       ( code_a
         @ [ Mv (temp_reg2, result_reg) ]
         @ code_b
         @ [ Mv (temp_reg3, result_reg) ]
         @ [ Srai (temp_reg2, temp_reg2, 1)
           ; Srai (temp_reg3, temp_reg3, 1)
           ; Slt (temp_reg, temp_reg2, temp_reg3)
           ; Slli (temp_reg, temp_reg, 1)
           ; Ori (temp_reg, temp_reg, 1)
           ; Mv (result_reg, temp_reg)
           ]
       , env )
     | LowestEqual ->
       ( code_a
         @ [ Mv (temp_reg2, result_reg) ]
         @ code_b
         @ [ Mv (temp_reg3, result_reg) ]
         @ [ Srai (temp_reg2, temp_reg2, 1)
           ; Srai (temp_reg3, temp_reg3, 1)
           ; Slt (temp_reg, temp_reg3, temp_reg2)
           ; Xori (temp_reg, temp_reg, 1)
           ; Slli (temp_reg, temp_reg, 1)
           ; Ori (temp_reg, temp_reg, 1)
           ; Mv (result_reg, temp_reg)
           ]
       , env )
     | Plus | Minus | Multiply ->
       ( code_a
         @ [ Mv (temp_reg2, result_reg) ]
         @ code_b
         @ [ Mv (temp_reg3, result_reg) ]
         @ [ Srai (temp_reg2, temp_reg2, 1); Srai (temp_reg3, temp_reg3, 1) ]
         @ gen_binop op
         @ [ Slli (temp_reg, temp_reg, 1); Mv (result_reg, temp_reg) ]
       , env )
     | _ -> raise (Compile_error "Binary operator not supported yet"))
  | ComplexBranch (cond, then_expr, else_expr) ->
    let l_else = fresh_else () in
    let l_end = fresh_end () in
    let cond_code = gen_immediate env cond in
    let then_code, env_then = gen_anf env then_expr in
    let else_code, env_else = gen_anf env else_expr in
    let final_env =
      { env with
        max_local_offset = max env_then.max_local_offset env_else.max_local_offset
      }
    in
    ( cond_code
      @ [ Srai (temp_reg, result_reg, 1); Beqz (temp_reg, l_else) ]
      @ then_code
      @ [ J l_end; Label l_else ]
      @ else_code
      @ [ Label l_end ]
    , final_env )
  | _ -> raise (Compile_error "Tuples/lists/options not implemented yet")

and gen_anf env = function
  | AnfExpr c -> gen_complex env c
  | AnfLet (Rec, f, rhs, body) ->
    (match rhs with
     | ComplexLambda _ ->
       let env' = { env with globals = StringSet.add f env.globals; self_var = Some f } in
       let body_code, final_env = gen_anf env' body in
       body_code, final_env
     | _ -> raise (Compile_error "Rec must be a lambda"))
  | AnfLet (NonRec, x, rhs, body) ->
    let rhs_code, _ = gen_complex env rhs in
    let var_offset = -(env.locals_base + env.next_local_offset) in
    let next_offset = env.next_local_offset + 8 in
    let env' =
      { env with
        locals = (x, var_offset) :: env.locals
      ; next_local_offset = next_offset
      ; max_local_offset = max env.max_local_offset next_offset
      }
    in
    let body_code, final_env = gen_anf env' body in
    rhs_code @ [ Sd (result_reg, var_offset, Fp) ] @ body_code, final_env

and compile_global_function name params body =
  let param_offsets =
    List.mapi
      (fun i pat ->
         match pat with
         | PatVariable x -> x, -((i * 8) + 16)
         | _ -> raise (Compile_error "Only variable parameters supported"))
      params
  in
  let env =
    { empty_env with
      params = param_offsets
    ; locals_base = 16
    ; next_local_offset = 0
    ; globals = StringSet.singleton name
    ; self_var = Some name
    }
  in
  let body_code, final_env = gen_anf env body in
  let locals_space = final_env.max_local_offset in
  let total_without_align = 16 + locals_space in
  let frame_size =
    if total_without_align mod 16 = 0
    then total_without_align
    else ((total_without_align / 16) + 1) * 16
  in
  let prologue =
    [ Align 2
    ; Label name
    ; Addi (Sp, Sp, -frame_size)
    ; Sd (Ra, frame_size - 8, Sp)
    ; Sd (Fp, frame_size - 16, Sp)
    ; Addi (Fp, Sp, frame_size)
    ]
  in
  let save_params_code =
    param_offsets
    |> List.mapi (fun i (_, offset) -> if i < 8 then [ Sd (A i, offset, Fp) ] else [])
    |> List.concat
  in
  prologue
  @ save_params_code
  @ body_code
  @ [ Mv (A 0, result_reg)
    ; Ld (Fp, frame_size - 16, Sp)
    ; Ld (Ra, frame_size - 8, Sp)
    ; Addi (Sp, Sp, frame_size)
    ; Ret
    ]
;;

let runtime_functions () =
  [ Label "malloc"
  ; Align 2
  ; Addi (A 1, A 0, 0)
  ; Li (A 0, 0)
  ; Li (A 2, 3)
  ; Li (A 3, 0x22)
  ; Li (A 4, -1)
  ; Li (A 5, 0)
  ; Li (A 7, 222)
  ; Ecall
  ; Ret
  ]
;;

let gen_program p =
  let state = { functions = []; data = []; globals = StringSet.empty; main_code = [] } in
  let extract_bindings = function
    | AnfValue (_, (name, rhs), _) -> [ name, rhs ]
    | AnfEval e ->
      let rec helper acc = function
        | AnfExpr _ -> acc
        | AnfLet (Rec, f, rhs, body) -> (f, AnfExpr rhs) :: helper acc body
        | AnfLet (NonRec, _, _, body) -> helper acc body
      in
      helper [] e
  in
  List.iter
    (fun prog_item ->
       let bindings = extract_bindings prog_item in
       List.iter
         (fun (name, _) -> state.globals <- StringSet.add name state.globals)
         bindings)
    p;
  List.iter
    (fun prog_item ->
       let bindings = extract_bindings prog_item in
       List.iter
         (fun (name, rhs) ->
            match rhs with
            | AnfExpr (ComplexLambda (params, body)) ->
              let func_code = compile_global_function name params body in
              state.functions <- state.functions @ func_code
            | _ -> ())
         bindings)
    p;
  List.iter
    (fun prog_item ->
       match prog_item with
       | AnfEval e ->
         let env = { empty_env with globals = state.globals; locals_base = 16 } in
         let main_code, _ = gen_anf env e in
         state.main_code <- main_code
       | _ -> ())
    p;
  let entry_point =
    [ Label "_start"; Align 2; Addi (Sp, Sp, -16); Sd (Ra, 8, Sp); Addi (Fp, Sp, 16) ]
    @ state.main_code
    @ [ Mv (A 0, result_reg)
      ; Ld (Ra, 8, Sp)
      ; Addi (Sp, Sp, 16)
      ; Srai (A 0, A 0, 1)
      ; Li (A 7, 93)
      ; Ecall
      ]
  in
  let all_functions = entry_point @ runtime_functions () @ state.functions in
  let all_globals = "_start" :: "malloc" :: StringSet.elements state.globals in
  { text = all_functions; data = []; global_funs = all_globals }
;;
