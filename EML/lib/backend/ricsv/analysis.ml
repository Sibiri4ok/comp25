(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Frontend.Ast
open Middleend.Anf

type function_layout =
  { func_name : string
  ; params : immediate list
  ; body : anf_expr
  ; slots_count : int
  }

type analysis_result =
  { arity_map : (string, int, String.comparator_witness) Map.t
  ; functions : function_layout list
  }

let rec slots_in_imm = function
  | ImmediateVar _ | ImmediateConst _ -> 0

and slots_in_cexpr = function
  | ComplexImmediate imm -> slots_in_imm imm
  | ComplexUnit -> 0
  | ComplexBinOper (_, left, right) -> slots_in_imm left + slots_in_imm right
  | ComplexUnarOper (_, imm) -> slots_in_imm imm
  | ComplexTuple (first, second, rest) ->
    List.fold_left (first :: second :: rest) ~init:0 ~f:(fun acc e ->
      acc + slots_in_imm e)
  | ComplexField (imm, _) -> slots_in_imm imm
  | ComplexList imm_list ->
    List.fold_left imm_list ~init:0 ~f:(fun acc e -> acc + slots_in_imm e)
  | ComplexApp (first, second, rest) ->
    List.fold_left (first :: second :: rest) ~init:0 ~f:(fun acc e ->
      acc + slots_in_imm e)
  | ComplexOption None -> 0
  | ComplexOption (Some imm) -> slots_in_imm imm
  | ComplexLambda (_, body) -> slots_in_anf body
  | ComplexBranch (cond, then_e, else_e) ->
    slots_in_imm cond + slots_in_anf then_e + slots_in_anf else_e

and slots_in_anf = function
  | AnfExpr cexp -> slots_in_cexpr cexp
  | AnfLet (_, _, cexp, cont) -> 1 + slots_in_cexpr cexp + slots_in_anf cont
;;

let rec params_of_anf = function
  | AnfExpr (ComplexLambda (pats, body)) ->
    let imms =
      List.filter_map pats ~f:(function
        | PatVariable id -> Some (ImmediateVar id)
        | _ -> None)
    in
    let rest, inner = params_of_anf body in
    imms @ rest, inner
  | other -> [], other
;;

let arity_map_of_program (program : anf_program) =
  List.fold
    program
    ~init:(Map.empty (module String))
    ~f:(fun map -> function
      | AnfValue (_, (fid, arity, _), and_binds) ->
        let map = Map.set map ~key:fid ~data:arity in
        List.fold and_binds ~init:map ~f:(fun acc (id, arity, _) ->
          Map.set acc ~key:id ~data:arity)
      | _ -> map)
;;

let analyze (program : anf_program) =
  let arity_map = arity_map_of_program program in
  let functions =
    List.filter_map program ~f:(function
      | AnfValue (_, (func_name, _arity, body), _) ->
        let params, body = params_of_anf body in
        Some { func_name; params; body; slots_count = slots_in_anf body }
      | AnfEval _ -> None)
  in
  { arity_map; functions }
;;
