(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast
open Middleend.Anf

let word_size = 8

type function_layout =
  { func_name : string
  ; asm_name : string
  ; params : immediate list
  ; body : anf_expr
  ; slots_count : int
  ; max_stack_args : int
  ; max_create_tuple_array_bytes : int
  }

type analysis_result =
  { arity_map : (string, int, Base.String.comparator_witness) Base.Map.t
  ; functions : function_layout list
  ; resolve : int -> string -> (string * int) option
  }

let arg_regs_count = 8

let rec slots_in_imm = function
  | ImmediateVar _ | ImmediateConst _ -> 0

and slots_in_cexpr = function
  | ComplexImmediate imm -> slots_in_imm imm
  | ComplexUnit -> 0
  | ComplexBinOper (_, left, right) -> slots_in_imm left + slots_in_imm right
  | ComplexUnarOper (_, imm) -> slots_in_imm imm
  | ComplexTuple (first, second, rest) ->
    let elts = first :: second :: rest in
    List.length elts + List.fold_left (fun acc e -> acc + slots_in_imm e) 0 elts
  | ComplexField (imm, _) -> slots_in_imm imm
  | ComplexList imm_list ->
    let n = List.length imm_list in
    n + List.fold_left (fun acc e -> acc + slots_in_imm e) 0 imm_list
  | ComplexApp (first, second, rest) ->
    (* +1 for curried-call intermediate; +1 per arg for spill_dangerous_args.
       +8 for spill_caller_saved_vars_to_frame at start of every invocation (can spill a0-a7).
       +N when nargs >= 2: margin so partial stays above argv (confirmed: overwrite → eml_applyN gets c=0x3). *)
    let args = first :: second :: rest in
    let nargs = List.length args in
    let extra = if nargs >= 2 then 12 else 0 in
    1 + 8 + nargs + extra + List.fold_left (fun acc e -> acc + slots_in_imm e) 0 args
  | ComplexOption None -> 0
  | ComplexOption (Some imm) -> slots_in_imm imm
  | ComplexLambda (_, body) -> slots_in_anf body
  | ComplexBranch (cond, then_e, else_e) ->
    1 + slots_in_imm cond + slots_in_anf then_e + slots_in_anf else_e

and slots_in_anf = function
  | AnfExpr cexp -> slots_in_cexpr cexp
  | AnfLet (_, _, cexp, cont) -> 1 + slots_in_cexpr cexp + slots_in_anf cont
;;

let rec max_stack_args_cexpr = function
  | ComplexImmediate _ | ComplexUnit -> 0
  | ComplexBinOper (_, left, right) ->
    max (max_stack_args_imm left) (max_stack_args_imm right)
  | ComplexUnarOper (_, imm) -> max_stack_args_imm imm
  | ComplexTuple (first, second, rest) ->
    List.fold_left
      (fun acc e -> max acc (max_stack_args_imm e))
      0
      (first :: second :: rest)
  | ComplexField (imm, _) -> max_stack_args_imm imm
  | ComplexList imm_list ->
    List.fold_left (fun acc e -> max acc (max_stack_args_imm e)) 0 imm_list
  | ComplexApp (_first, second, rest) ->
    let nargs = 1 + List.length rest in
    (* Reserve enough for largest call: eml_applyN needs nargs words; direct needs max(0, nargs-8). *)
    let need = nargs in
    let in_args =
      List.fold_left (fun acc e -> max acc (max_stack_args_imm e)) 0 (second :: rest)
    in
    max need in_args
  | ComplexOption None -> 0
  | ComplexOption (Some imm) -> max_stack_args_imm imm
  | ComplexLambda (_, body) -> max_stack_args_anf body
  | ComplexBranch (cond, then_e, else_e) ->
    max
      (max_stack_args_imm cond)
      (max (max_stack_args_anf then_e) (max_stack_args_anf else_e))

and max_stack_args_imm = function
  | ImmediateVar _ | ImmediateConst _ -> 0

and max_stack_args_anf = function
  | AnfExpr cexp -> max_stack_args_cexpr cexp
  | AnfLet (_, _, cexp, cont) -> max (max_stack_args_cexpr cexp) (max_stack_args_anf cont)
;;

let rec max_create_tuple_array_cexpr = function
  | ComplexImmediate _ | ComplexUnit -> 0
  | ComplexBinOper (_, left, right) ->
    max (max_create_tuple_array_imm left) (max_create_tuple_array_imm right)
  | ComplexUnarOper (_, imm) -> max_create_tuple_array_imm imm
  | ComplexTuple (first, second, rest) ->
    let elts = first :: second :: rest in
    let here = List.length elts * word_size in
    List.fold_left (fun acc e -> max acc (max_create_tuple_array_imm e)) here elts
  | ComplexField (imm, _) -> max_create_tuple_array_imm imm
  | ComplexList imm_list ->
    (* Each cons adds 16 bytes; they accumulate along the list build *)
    let per_cons = 2 * word_size in
    let from_elts =
      List.fold_left (fun acc e -> acc + max_create_tuple_array_imm e) 0 imm_list
    in
    (per_cons * List.length imm_list) + from_elts
  | ComplexApp (_f, second, rest) ->
    List.fold_left (fun acc e -> max acc (max_create_tuple_array_imm e)) 0 (second :: rest)
  | ComplexOption None -> 0
  | ComplexOption (Some imm) -> max_create_tuple_array_imm imm
  | ComplexLambda (_, body) -> max_create_tuple_array_anf body
  | ComplexBranch (cond, then_e, else_e) ->
    max
      (max_create_tuple_array_imm cond)
      (max (max_create_tuple_array_anf then_e) (max_create_tuple_array_anf else_e))

and max_create_tuple_array_imm = function
  | ImmediateVar _ | ImmediateConst _ -> 0

and max_create_tuple_array_anf = function
  | AnfExpr cexp -> max_create_tuple_array_cexpr cexp
  | AnfLet (_, _, cexp, cont) ->
    max (max_create_tuple_array_cexpr cexp) (max_create_tuple_array_anf cont)
;;

let rec params_of_anf = function
  | AnfExpr (ComplexLambda (pats, body)) ->
    let imms =
      List.filter_map
        (function
          | PatVariable id -> Some (ImmediateVar id)
          | _ -> None)
        pats
    in
    let rest, inner = params_of_anf body in
    imms @ rest, inner
  | other -> [], other
;;

let arity_map_of_program (program : anf_program) =
  List.fold_left
    (fun map -> function
       | AnfValue (_, (fid, arity, _), and_binds) ->
         let map = Base.Map.set map ~key:fid ~data:arity in
         List.fold_left
           (fun acc (id, arity, _) -> Base.Map.set acc ~key:id ~data:arity)
           map
           and_binds
       | _ -> map)
    (Base.Map.empty (module Base.String))
    program
;;

let analyze (program : anf_program) =
  let arity_map = arity_map_of_program program in
  let raw =
    List.filter_map
      (function
        | AnfValue (_, (func_name, arity, body), _) ->
          let params, body = params_of_anf body in
          Some
            ( func_name
            , arity
            , params
            , body
            , slots_in_anf body
            , max_stack_args_anf body
            , max_create_tuple_array_anf body )
        | AnfEval _ -> None)
      program
  in
  let counts = ref (Base.Map.empty (module Base.String)) in
  let mangle_reserved name = if String.equal name "_start" then "eml_start" else name in
  let asm_name name =
    let base = mangle_reserved name in
    let n = Base.Map.find !counts name |> Option.value ~default:0 in
    counts := Base.Map.set !counts ~key:name ~data:(n + 1);
    if n = 0 then base else base ^ "_" ^ Int.to_string n
  in
  let functions =
    List.map
      (fun ( func_name
           , _arity
           , params
           , body
           , slots_count
           , max_stack_args
           , max_create_tuple_array_bytes ) ->
         { func_name
         ; asm_name = asm_name func_name
         ; params
         ; body
         ; slots_count
         ; max_stack_args
         ; max_create_tuple_array_bytes
         })
      raw
  in
  let has_main = List.exists (fun fn -> String.equal fn.func_name "main") functions in
  let functions =
    if has_main
    then functions
    else (
      let synthetic_main =
        { func_name = "main"
        ; asm_name = "main"
        ; params = []
        ; body = AnfExpr (ComplexImmediate (ImmediateConst (ConstInt 0)))
        ; slots_count = 0
        ; max_stack_args = 0
        ; max_create_tuple_array_bytes = 0
        }
      in
      functions @ [ synthetic_main ])
  in
  let arity_map =
    if has_main then arity_map else Base.Map.set arity_map ~key:"main" ~data:0
  in
  let resolver func_index var_name =
    let rec find i =
      if i < 0
      then None
      else (
        match Base.List.nth functions i with
        | None -> None
        | Some fn when String.equal fn.func_name var_name ->
          Some (fn.asm_name, List.length fn.params)
        | Some _ -> find (i - 1))
    in
    find (func_index - 1)
  in
  { arity_map; functions; resolve = resolver }
;;
