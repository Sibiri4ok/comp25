[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend.Ast
open Base

module ANFMonad = struct
  type 'a t = int -> int * ('a, string) Result.t

  let return x = fun counter -> counter, Ok x

  let ( >>= ) m f =
    fun counter ->
    match m counter with
    | counter', Ok a -> f a counter'
    | counter', Error e -> counter', Error e
  ;;

  let fresh : string t = fun counter -> counter + 1, Ok ("anf_t" ^ Int.to_string counter)
  let run m = m 0 |> snd
  let fail msg = fun counter -> counter, Error msg

  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end

open ANFMonad
open ANFMonad.Syntax
type immediate =
  | ImmediateConst of const
  | ImmediateVar of ident
[@@deriving show { with_path = false }]

type complex_expr =
  | ComplexImmediate of immediate
  | ComplexUnit
  | ComplexBinOper of bin_oper * immediate * immediate
  | ComplexUnarOper of unar_oper * immediate
  | ComplexTuple of immediate * immediate * immediate list
  | ComplexField of immediate * int
  | ComplexList of immediate list
  | ComplexOption of immediate option
  | ComplexApp of immediate * immediate * immediate list
  | ComplexLambda of pattern list * anf_expr
  | ComplexBranch of immediate * anf_expr * anf_expr
[@@deriving show { with_path = false }]

and anf_expr =
  | AnfLet of is_rec * ident * complex_expr * anf_expr
  | AnfExpr of complex_expr
[@@deriving show { with_path = false }]

type anf_bind = ident * anf_expr [@@deriving show { with_path = false }]

type anf_structure =
  | AnfEval of anf_expr
  | AnfValue of is_rec * anf_bind * anf_bind list
[@@deriving show { with_path = false }]

type anf_program = anf_structure list [@@deriving show { with_path = false }]




let optimize_anf_let (is_rec, name1, expr, body) =
  match is_rec, body with
  | NonRec, AnfExpr (ComplexImmediate (ImmediateVar name2)) when String.equal name1 name2
    -> AnfExpr expr
  | _, AnfLet (is_rec', orig_name, ComplexImmediate (ImmediateVar name2), body')
    when String.equal name1 name2
    -> AnfLet (is_rec', orig_name, expr, body')
  | _ -> AnfLet (is_rec, name1, expr, body)
;;

let bind_complex_expr complex_expr k =
  let* var = fresh in
  let* body_expr = k (ImmediateVar var) in
  return (optimize_anf_let (NonRec, var, complex_expr, body_expr))
;;

let get_var = function
  | PatVariable id -> return id
  | _ -> fresh 
;;

let rec destructure_tuple_pat tuple_var indices_pats empty nested_empty add 
  =
  match indices_pats with
  | [] -> return empty
  | (i, pat) :: rest ->
    let* var = get_var pat in
    let* rest_result = destructure_tuple_pat tuple_var rest empty nested_empty add in
    let* inner_result =
      (match pat with
       | PatTuple (ip1, ip2, irest) ->
         destructure_tuple_pat var
           (List.mapi (ip1 :: ip2 :: irest) ~f:(fun j p -> j, p))
           (nested_empty rest_result)
           nested_empty add
       | _ -> return (nested_empty rest_result))
    in
    return (add var i tuple_var inner_result rest_result)
;;

let build_tuple_lets tuple_var indices_pats body =
  destructure_tuple_pat tuple_var indices_pats body (fun x -> x) (fun bind_id i tv inner _rest ->
    AnfLet (NonRec, bind_id, ComplexField (ImmediateVar tv, i), inner))
;;

let build_tuple_top_level_bindings tuple_var indices_pats = 
  destructure_tuple_pat tuple_var indices_pats [] (fun _ -> []) (fun bind_id i tv inner rest ->
    (bind_id, AnfExpr (ComplexField (ImmediateVar tv, i))) :: inner @ rest)
;;

let rec anf (expr : expr) (k : immediate -> anf_expr t) : anf_expr t =
  match expr with
  | ExpConst c -> k (ImmediateConst c)
  | ExpIdent x -> k (ImmediateVar x)
  | ExpUnarOper (op, expr) ->
    anf expr (fun imm ->
      bind_complex_expr (ComplexUnarOper (op, imm)) k)
  | ExpBinOper (op, exp1, exp2) ->
    anf exp1 (fun imm1 ->
      anf exp2 (fun imm2 ->
        bind_complex_expr (ComplexBinOper (op, imm1, imm2)) k))
  
  | ExpBranch (cond, then_exp, else_exp_opt) ->
    anf cond (fun imm_cond ->
      let* then_aexp = anf then_exp (fun imm -> return (AnfExpr (ComplexImmediate imm))) in
      let* else_aexp =
        match else_exp_opt with
        | None -> return (AnfExpr ComplexUnit)
        | Some else_exp -> anf else_exp (fun imm -> return (AnfExpr (ComplexImmediate imm)))
      in
      bind_complex_expr (ComplexBranch (imm_cond, then_aexp, else_aexp)) k)
        
  | ExpLet (flag, (pat, expr), _, body) ->
    (match pat with
     | PatAny | PatConstruct ("()", None) -> anf expr (fun _ -> anf body k)
     | PatTuple (p1, p2, rest) ->
       let pats = p1 :: p2 :: rest in
       anf expr (fun tuple_imm ->
         let* tuple_var = fresh in
         let* body_anf_expr = anf body k in
         let* with_lets =
           build_tuple_lets tuple_var (List.mapi pats ~f:(fun i p -> i, p)) body_anf_expr
         in
         return (AnfLet (flag, tuple_var, ComplexImmediate tuple_imm, with_lets)))
     | PatVariable _ | PatConst _ ->
       anf expr (fun imm ->
         let* body_anf_expr = anf body k in
         let* var = get_var pat in
         return (AnfLet (flag, var, ComplexImmediate imm, body_anf_expr)))
     | _ -> fail "Complex patterns in let not supported")

  | ExpApply (exp1, exp2) ->
    let func, args_list =
      let rec collect_args acc = function
        | ExpApply (f, arg) -> collect_args (arg :: acc) f
        | f -> f, acc
      in
      collect_args [] (ExpApply (exp1, exp2))
    in
    anf func (fun immediate_func ->
      anf_list args_list (function
        | arg1 :: arg_tl -> bind_complex_expr (ComplexApp (immediate_func, arg1, arg_tl)) k
        | [] -> fail "application with no arguments"))
  | ExpTuple (exp1, exp2, exp_list) ->
    let all_exprs = exp1 :: exp2 :: exp_list in
    anf_list all_exprs (fun imm_list ->
      match imm_list with
      | imm1 :: imm2 :: rest ->
        bind_complex_expr (ComplexTuple (imm1, imm2, rest)) k
      | _ -> fail "Invalid tuple")
  | ExpLambda (pat, pat_list, body) ->
    let params = pat :: pat_list in
    let* body_anf_expr = anf body (fun imm -> bind_complex_expr (ComplexImmediate imm) k) in
    let rec wrap_params current_body = function
      | [] -> return current_body
      | (PatVariable _ | PatConst _) as param :: remaining_params ->
        let* body_with_rest = wrap_params current_body remaining_params in
        return (AnfExpr (ComplexLambda ([ param ], body_with_rest)))
      | PatTuple (p1, p2, rest_pats) :: remaining_params ->
        let* body_with_rest = wrap_params current_body remaining_params in
        let* var = fresh in
        let* body_with_tuple_destructured =
          build_tuple_lets var
            (List.mapi (p1 :: p2 :: rest_pats) ~f:(fun i p -> i, p))
            body_with_rest
        in
        return (AnfExpr (ComplexLambda ([ PatVariable var ], body_with_tuple_destructured)))
      | _ -> fail "Only variable, constant and tuple patterns in lambda"
    in
    wrap_params body_anf_expr params
  | ExpConstruct ("()", None) -> bind_complex_expr ComplexUnit k
  | ExpTypeAnnotation (e, _) -> anf e k
  | ExpList exprs ->
    anf_list exprs (fun imm_list ->
      bind_complex_expr (ComplexList imm_list) k)
  | ExpOption None -> bind_complex_expr ComplexUnit k
  | ExpOption (Some e) -> anf e k
  | _ -> fail "Exp: Not implemented"

and anf_list (exprs : expr list) (k : immediate list -> anf_expr t) : anf_expr t =
  match exprs with
  | [] -> k []
  | hd :: tl ->
    anf hd (fun immediate_hd ->
      anf_list tl (fun immediate_tl -> k (immediate_hd :: immediate_tl)))
;;

let anf_structure_item (item : structure) : anf_structure list t =
  match item with
  | SEval expr ->
    let* result =
      anf expr (fun immediate -> return (AnfExpr (ComplexImmediate immediate)))
    in
    return [ AnfEval result ]
  | SValue (rec_flag, (pat, expr), binds) ->
    let bindings = (pat, expr) :: binds in
    List.fold_left bindings ~init:(return []) ~f:(fun acc (pat, expr) ->
      let* acc_list = acc in
      let* anf_expr_body =
        anf expr (fun immediate -> return (AnfExpr (ComplexImmediate immediate)))
      in
      match pat with
      | PatTuple (p1, p2, rest) ->
        let* tuple_var = fresh in
        let* component_bindings =
          build_tuple_top_level_bindings tuple_var
            (List.mapi (p1 :: p2 :: rest) ~f:(fun i p -> i, p))
        in
        let one_value (id, e) = AnfValue (NonRec, (id, e), []) in
        let new_items =
          AnfValue (rec_flag, (tuple_var, anf_expr_body), [])
          :: List.map component_bindings ~f:one_value
        in
        return (acc_list @ new_items)
      | _ ->
        let* var = get_var pat in
        return (acc_list @ [ AnfValue (rec_flag, (var, anf_expr_body), []) ]))
;;

let anf_program (program : program) : (anf_program, string) Result.t =
  let program' =
    List.fold_left program ~init:(return []) ~f:(fun acc item ->
      let* acc_list = acc in
      let* item_anf = anf_structure_item item in
      return (acc_list @ item_anf))
  in
  ANFMonad.run program'