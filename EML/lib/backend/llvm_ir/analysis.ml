(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast
open Middleend.Anf

type function_layout =
  { func_name : string
  ; asm_name : string
  ; params : immediate list
  ; body : anf_expr
  ; slots_count : int
  }

type analysis_result =
  { arity_map : (string, int, Base.String.comparator_witness) Base.Map.t
  ; functions : function_layout list
  ; resolve : int -> string -> (string * int) option
  }

let rec slots_in_imm = function
  | ImmediateVar _ | ImmediateConst _ -> 0

and slots_in_cexpr = function
  | ComplexImmediate imm -> slots_in_imm imm
  | ComplexUnit -> 0
  | ComplexBinOper (_, left, right) -> slots_in_imm left + slots_in_imm right
  | ComplexUnarOper (_, imm) -> slots_in_imm imm
  | ComplexTuple (first, second, rest) ->
    List.fold_left
      (fun slot_count expr -> slot_count + slots_in_imm expr)
      0
      (first :: second :: rest)
  | ComplexField (imm, _) -> slots_in_imm imm
  | ComplexList imm_list ->
    List.fold_left (fun slot_count expr -> slot_count + slots_in_imm expr) 0 imm_list
  | ComplexApp (_, second, rest) ->
    1
    + List.fold_left
        (fun slot_count expr -> slot_count + slots_in_imm expr)
        0
        (second :: rest)
  | ComplexOption None -> 0
  | ComplexOption (Some imm) -> slots_in_imm imm
  | ComplexLambda (_, body) -> slots_in_anf body
  | ComplexBranch (cond, then_expr, else_expr) ->
    slots_in_imm cond + slots_in_anf then_expr + slots_in_anf else_expr

and slots_in_anf = function
  | AnfExpr cexp -> slots_in_cexpr cexp
  | AnfLet (_, _, cexp, cont) -> 1 + slots_in_cexpr cexp + slots_in_anf cont
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
          Some (func_name, arity, params, body, slots_in_anf body)
        | AnfEval _ -> None)
      program
  in
  let mangle_reserved name = if String.equal name "_start" then "eml_start" else name in
  let functions, _ =
    List.fold_left
      (fun (reversed_functions, counts) (func_name, _arity, params, body, slots_count) ->
         let base_asm_name = mangle_reserved func_name in
         let duplicate_index =
           Base.Map.find counts func_name |> Option.value ~default:0
         in
         let updated_counts =
           Base.Map.set counts ~key:func_name ~data:(duplicate_index + 1)
         in
         let asm_name =
           if duplicate_index = 0
           then base_asm_name
           else base_asm_name ^ "_" ^ Int.to_string duplicate_index
         in
         ( { func_name; asm_name; params; body; slots_count } :: reversed_functions
         , updated_counts ))
      ([], Base.Map.empty (module Base.String))
      raw
  in
  let functions = List.rev functions in
  let has_main =
    List.exists (fun func_layout -> String.equal func_layout.func_name "main") functions
  in
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
        | Some func_layout when String.equal func_layout.func_name var_name ->
          Some (func_layout.asm_name, List.length func_layout.params)
        | Some _ -> find (i - 1))
    in
    find (func_index - 1)
  in
  { arity_map; functions; resolve = resolver }
;;
