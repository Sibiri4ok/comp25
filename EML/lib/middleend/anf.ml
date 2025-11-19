(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend
open Ast
open Base
open Utils.Aux
open Utils.Monads.ANFMonad
open Syntax
open Anf_tree

let optimize_anf_let rf name1 v body =
  match rf, body with
  | NonRec, AnfExpr (ComplexImmediate (ImmediateVar name2)) when String.equal name1 name2
    -> AnfExpr v
  | _ -> AnfLet (rf, name1, v, body)
;;

let rec anf (e : expr) (k : immediate -> anf_expr t) : anf_expr t =
  match e with
  | ExpConst c -> k (ImmediateConst c)
  | ExpIdent x -> k (ImmediateVar x)
  | ExpUnarOper (op, e) ->
    anf e (fun immediate ->
      let* var_name = fresh in
      let* cont_expr = k (ImmediateVar var_name) in
      return (AnfLet (NonRec, var_name, ComplexUnarOper (op, immediate), cont_expr)))
  | ExpBinOper (op, e1, e2) ->
    anf e1 (fun immediate1 ->
      anf e2 (fun immediate2 ->
        let* var_name = fresh in
        let* cont_expr = k (ImmediateVar var_name) in
        return
          (AnfLet
             (NonRec, var_name, ComplexBinOper (op, immediate1, immediate2), cont_expr))))
  | ExpTuple (e1, e2, rest) ->
    let all_exprs = e1 :: e2 :: rest in
    anf_list all_exprs (fun imms ->
      match imms with
      | i1 :: i2 :: rest_imm ->
        let* var_name = fresh in
        let* cont_expr = k (ImmediateVar var_name) in
        return (AnfLet (NonRec, var_name, ComplexTuple (i1, i2, rest_imm), cont_expr))
      | _ -> fail "Invalid tuple")
  | ExpList exprs ->
    anf_list exprs (fun imms ->
      let* var_name = fresh in
      let* cont_expr = k (ImmediateVar var_name) in
      return (AnfLet (NonRec, var_name, ComplexList imms, cont_expr)))
  | ExpOption opt_expr ->
    let* var_name = fresh in
    let* cont_expr = k (ImmediateVar var_name) in
    (match opt_expr with
     | None -> return (AnfLet (NonRec, var_name, ComplexOption None, cont_expr))
     | Some expr ->
       anf expr (fun immediate ->
         return (AnfLet (NonRec, var_name, ComplexOption (Some immediate), cont_expr))))
  | ExpBranch (cond, then_expr, else_expr) ->
    anf cond (fun immediate_cond ->
      let* then_anf =
        anf then_expr (fun immediate -> return (AnfExpr (ComplexImmediate immediate)))
      in
      let* var_name = fresh in
      let* cont_expr = k (ImmediateVar var_name) in
      let* else_anf =
        match else_expr with
        | Some e -> anf e (fun immediate -> return (AnfExpr (ComplexImmediate immediate)))
        | None -> return (AnfExpr (ComplexImmediate (ImmediateConst (ConstBool false))))
      in
      return
        (AnfLet
           ( NonRec
           , var_name
           , ComplexBranch (immediate_cond, then_anf, else_anf)
           , cont_expr )))
  | ExpLet (rec_flag, (pat, e1), _, e2) ->
    let* e1_anf =
      anf e1 (fun immediate -> return (AnfExpr (ComplexImmediate immediate)))
    in
    let* e2_anf = anf e2 k in
    let* complex_expr_body =
      match e1_anf with
      | AnfExpr c -> return c
      | _ -> fail "Expected complex_expr"
    in
    if is_simple_pattern pat
    then (
      match pattern_to_ident pat with
      | Some name -> return (AnfLet (rec_flag, name, complex_expr_body, e2_anf))
      | None ->
        let* var_name = fresh in
        return (AnfLet (NonRec, var_name, complex_expr_body, e2_anf)))
    else fail "Complex patterns in let bindings not yet supported"
  | ExpLambda (patterns, body) ->
    let* body_anf =
      anf body (fun immediate -> return (AnfExpr (ComplexImmediate immediate)))
    in
    let* var_name = fresh in
    let* cont_expr = k (ImmediateVar var_name) in
    return (AnfLet (NonRec, var_name, ComplexLambda (patterns, body_anf), cont_expr))
  | ExpFunction (func, arg) ->
    anf func (fun immediate_func ->
      anf arg (fun immediate_arg ->
        let* var_name = fresh in
        let* cont_expr = k (ImmediateVar var_name) in
        return
          (AnfLet
             (NonRec, var_name, ComplexApp (immediate_func, immediate_arg, []), cont_expr))))
  | ExpTypeAnnotation (e, _) -> anf e k

and anf_list (exprs : expr list) (k : immediate list -> anf_expr t) : anf_expr t =
  match exprs with
  | [] -> k []
  | hd :: tl ->
    anf hd (fun immediate_hd ->
      anf_list tl (fun immediate_tl -> k (immediate_hd :: immediate_tl)))
;;

let anf_structure_item (item : structure) : anf_structure t =
  match item with
  | SEval expr ->
    let* result =
      anf expr (fun immediate -> return (AnfExpr (ComplexImmediate immediate)))
    in
    return (AnfEval result)
  | SValue (rec_flag, (pat, expr), _) ->
    if is_simple_pattern pat
    then
      let* anf_expr_body =
        anf expr (fun immediate -> return (AnfExpr (ComplexImmediate immediate)))
      in
      match pattern_to_ident pat with
      | Some name -> return (AnfValue (rec_flag, (name, anf_expr_body), []))
      | None -> return (AnfValue (rec_flag, ("_", anf_expr_body), []))
    else fail "Complex patterns in top-level bindings not yet supported"
;;

let anf_program (program : program) : (anf_program, string) Result.t =
  let program' =
    List.fold_right program ~init:(return []) ~f:(fun item acc ->
      let* acc_list = acc in
      let* item_anf = anf_structure_item item in
      return (item_anf :: acc_list))
  in
  run program'
;;

(* Function to convert ANF expression to string using the pretty-printer *)
let anf_to_string anf_program = Stdlib.Format.asprintf "%a" pp_anf_program anf_program
let string_of_anf_expr anf_expr = Stdlib.Format.asprintf "%a" pp_anf_expr anf_expr

let string_of_complex_expr complex_expr =
  Stdlib.Format.asprintf "%a" pp_complex_expr complex_expr
;;

let string_of_immediate immediate = Stdlib.Format.asprintf "%a" pp_immediate immediate
