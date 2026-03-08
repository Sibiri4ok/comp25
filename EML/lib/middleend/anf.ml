[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend.Ast
open Base
open Util.Monads.ANFMonad
open Util.Monads.ANFMonad.Syntax

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

type arity = int

let pp_arity ppf (n : arity) = Stdlib.Format.pp_print_int ppf n

type anf_bind = ident * anf_expr [@@deriving show { with_path = false }]
type anf_fun_bind = ident * arity * anf_expr [@@deriving show { with_path = false }]

type anf_structure =
  | AnfEval of anf_expr
  | AnfValue of is_rec * anf_fun_bind * anf_fun_bind list
[@@deriving show { with_path = false }]

type anf_program = anf_structure list [@@deriving show { with_path = false }]

let rec anf_expr_arity = function
  | AnfExpr (ComplexLambda (pat_list, body)) -> List.length pat_list + anf_expr_arity body
  | AnfLet (_, _, _, body) -> anf_expr_arity body
  | _ -> 0
;;

let optimize_anf_let (is_rec, name1, expr, body) =
  match is_rec, body with
  | NonRec, AnfExpr (ComplexImmediate (ImmediateVar name2)) when String.equal name1 name2
    -> AnfExpr expr
  | _, AnfLet (is_rec', orig_name, ComplexImmediate (ImmediateVar name2), body')
    when String.equal name1 name2 -> AnfLet (is_rec', orig_name, expr, body')
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

let rec destructure_tuple_pat tuple_var indices_pats empty nested_empty add =
  match indices_pats with
  | [] -> return empty
  | (i, pat) :: rest ->
    let* var = get_var pat in
    let* rest_result = destructure_tuple_pat tuple_var rest empty nested_empty add in
    let* inner_result =
      match pat with
      | PatTuple (ip1, ip2, irest) ->
        destructure_tuple_pat
          var
          (List.mapi (ip1 :: ip2 :: irest) ~f:(fun j p -> j, p))
          (nested_empty rest_result)
          nested_empty
          add
      | _ -> return (nested_empty rest_result)
    in
    return (add var i tuple_var inner_result rest_result)
;;

let build_tuple_lets tuple_var indices_pats body =
  destructure_tuple_pat
    tuple_var
    indices_pats
    body
    (fun x -> x)
    (fun bind_id i tv inner _rest ->
       AnfLet (NonRec, bind_id, ComplexField (ImmediateVar tv, i), inner))
;;

let build_tuple_top_level_bindings tuple_var indices_pats =
  destructure_tuple_pat
    tuple_var
    indices_pats
    []
    (fun _ -> [])
    (fun bind_id i tv inner rest ->
       ((bind_id, AnfExpr (ComplexField (ImmediateVar tv, i))) :: inner) @ rest)
;;

let bind_pattern_to_var pattern var body_expr =
  ExpLet (NonRec, (pattern, ExpIdent var), [], body_expr)
;;

let immediate_to_anf imm = AnfExpr (ComplexImmediate imm)
let tuple_indexed_patterns p1 p2 rest = List.mapi (p1 :: p2 :: rest) ~f:(fun i p -> i, p)

let parse_list_case_pattern = function
  | PatConstruct ("[]", None) -> Ok (`Nil)
  | PatConstruct ("::", Some (PatTuple (p_hd, p_tl, []))) -> Ok (`Cons (p_hd, p_tl))
  | _ -> Error "Only [] and h::tl list patterns are supported in match"
;;

let build_list_match_branch scrut_imm nil_anf cons_anf =
  let* scrut_var = fresh in
  let* tag_var = fresh in
  let* hd_var = fresh in
  let* tl_var = fresh in
  let* cond_var = fresh in
  return
    (AnfLet
       ( NonRec
       , scrut_var
       , ComplexImmediate scrut_imm
       , AnfLet
           ( NonRec
           , tag_var
           , ComplexField (ImmediateVar scrut_var, 0)
           , AnfLet
               ( NonRec
               , hd_var
               , ComplexField (ImmediateVar scrut_var, 1)
               , AnfLet
                   ( NonRec
                   , tl_var
                   , ComplexField (ImmediateVar scrut_var, 2)
                   , AnfLet
                       ( NonRec
                       , "__list_hd"
                       , ComplexImmediate (ImmediateVar hd_var)
                       , AnfLet
                           ( NonRec
                           , "__list_tl"
                           , ComplexImmediate (ImmediateVar tl_var)
                           , AnfLet
                               ( NonRec
                               , cond_var
                               , ComplexBinOper
                                   (Equal, ImmediateVar tag_var, ImmediateConst (ConstInt 0))
                               , AnfExpr
                                   (ComplexBranch
                                      (ImmediateVar cond_var, nil_anf, cons_anf)) ) ) ) ) ) ) ))
;;

let rec anf (expr : expr) (k : immediate -> anf_expr t) : anf_expr t =
  match expr with
  | ExpConst c -> k (ImmediateConst c)
  | ExpIdent x -> k (ImmediateVar x)
  | ExpUnarOper (op, expr) ->
    anf expr (fun imm -> bind_complex_expr (ComplexUnarOper (op, imm)) k)
  | ExpBinOper (op, exp1, exp2) ->
    anf exp1 (fun imm1 ->
      anf exp2 (fun imm2 -> bind_complex_expr (ComplexBinOper (op, imm1, imm2)) k))
  | ExpBranch (cond, then_exp, else_exp_opt) ->
    anf cond (fun imm_cond ->
      let* then_aexp = anf_as_expr then_exp in
      let* else_aexp =
        match else_exp_opt with
        | None -> return (AnfExpr ComplexUnit)
        | Some else_exp -> anf_as_expr else_exp
      in
      bind_complex_expr (ComplexBranch (imm_cond, then_aexp, else_aexp)) k)
  | ExpLet (flag, (pat, expr), _, body) ->
    (match pat with
     | PatAny | PatUnit | PatConstruct ("()", None) -> anf expr (fun _ -> anf body k)
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
        | arg1 :: arg_tl ->
          bind_complex_expr (ComplexApp (immediate_func, arg1, arg_tl)) k
        | [] -> fail "application with no arguments"))
  | ExpTuple (exp1, exp2, exp_list) ->
    let all_exprs = exp1 :: exp2 :: exp_list in
    anf_list all_exprs (fun imm_list ->
      match imm_list with
      | imm1 :: imm2 :: rest -> bind_complex_expr (ComplexTuple (imm1, imm2, rest)) k
      | _ -> fail "Invalid tuple")
  | ExpLambda (pat, pat_list, body) ->
    let params = pat :: pat_list in
    let* body_anf_expr = anf_as_expr body in
    let rec wrap_params current_body = function
      | [] -> return current_body
      | (PatAny | PatUnit | PatConstruct ("()", None)) :: remaining_params ->
        let* body_with_rest = wrap_params current_body remaining_params in
        let* var = fresh in
        return (AnfExpr (ComplexLambda ([ PatVariable var ], body_with_rest)))
      | ((PatVariable _ | PatConst _) as param) :: remaining_params ->
        let* body_with_rest = wrap_params current_body remaining_params in
        return (AnfExpr (ComplexLambda ([ param ], body_with_rest)))
      | PatTuple (p1, p2, rest_pats) :: remaining_params ->
        let* body_with_rest = wrap_params current_body remaining_params in
        let* var = fresh in
        let* body_with_tuple_destructured =
          build_tuple_lets
            var
            (tuple_indexed_patterns p1 p2 rest_pats)
            body_with_rest
        in
        return
          (AnfExpr (ComplexLambda ([ PatVariable var ], body_with_tuple_destructured)))
      | _ -> fail "Only variable, constant and tuple patterns in lambda"
    in
    let* lambda_anf = wrap_params body_anf_expr params in
    (match lambda_anf with
     | AnfExpr (ComplexLambda (pats, body)) ->
       bind_complex_expr (ComplexLambda (pats, body)) k
     | _ -> fail "ExpLambda: wrap_params must return ComplexLambda")
  | ExpConstruct ("()", None) -> bind_complex_expr ComplexUnit k
  | ExpTypeAnnotation (e, _) -> anf e k
  | ExpList exprs ->
    anf_list exprs (fun imm_list -> bind_complex_expr (ComplexList imm_list) k)
  | ExpOption None -> bind_complex_expr ComplexUnit k
  | ExpOption (Some e) -> anf e k
  | ExpFunction _ -> fail "Match/function cases not implemented"
  | ExpMatch (scrut, (pat_first, expr_first), rest_cases) ->
    let all_cases = (pat_first, expr_first) :: rest_cases in
    let classify_case (pat, expr) =
      match parse_list_case_pattern pat with
      | Ok `Nil -> Ok (`NilCase expr)
      | Ok (`Cons (p_hd, p_tl)) -> Ok (`ConsCase (p_hd, p_tl, expr))
      | Error msg -> Error msg
    in
    let* nil_case, cons_case =
      List.fold_left
        all_cases
        ~init:(return (None, None))
        ~f:(fun acc case ->
          let* nil_acc, cons_acc = acc in
          match classify_case case with
          | Error msg -> fail msg
          | Ok (`NilCase expr) ->
            if Option.is_some nil_acc then fail "Duplicate [] case in list match"
            else return (Some expr, cons_acc)
          | Ok (`ConsCase (p_hd, p_tl, expr)) ->
            if Option.is_some cons_acc then fail "Duplicate h::tl case in list match"
            else return (nil_acc, Some (p_hd, p_tl, expr)))
    in
    (match nil_case, cons_case with
     | Some nil_expr, Some (p_hd, p_tl, cons_expr) ->
       anf scrut (fun scrut_imm ->
         let* nil_anf = anf_as_expr nil_expr in
         let* cons_anf =
           let cons_wrapped =
             bind_pattern_to_var
               p_hd
               "__list_hd"
               (bind_pattern_to_var p_tl "__list_tl" cons_expr)
           in
           anf_as_expr cons_wrapped
         in
         build_list_match_branch scrut_imm nil_anf cons_anf)
     | _ -> fail "List match requires exactly two cases: [] and h::tl")
  | ExpConstruct ("[]", None) ->
    bind_complex_expr
      (ComplexTuple (ImmediateConst (ConstInt 0), ImmediateConst (ConstInt 0), []))
      k
  | ExpConstruct ("::", Some e) ->
    (match e with
     | ExpTuple (exp_hd, exp_tl, []) ->
       anf exp_hd (fun imm_hd ->
         anf exp_tl (fun imm_tl ->
           bind_complex_expr
             (ComplexTuple (ImmediateConst (ConstInt 1), imm_hd, [ imm_tl ]))
             k))
     | _ -> fail "List constructor :: expects tuple (head, tail)")
  | ExpConstruct _ -> fail "Constructors not implemented"

and anf_as_expr e = anf e (fun imm -> return (immediate_to_anf imm))

and anf_list (exprs : expr list) (k : immediate list -> anf_expr t) : anf_expr t =
  match exprs with
  | [] -> k []
  | hd :: tl ->
    anf hd (fun immediate_hd ->
      anf_list tl (fun immediate_tl -> k (immediate_hd :: immediate_tl)))
;;

let to_fun_bind (id, e) = id, anf_expr_arity e, e

let anf_structure_item (item : structure) : anf_structure list t =
  match item with
  | SEval expr ->
    let* result = anf_as_expr expr in
    return [ AnfEval result ]
  | SValue (rec_flag, (pat, expr), binds) ->
    let bindings = (pat, expr) :: binds in
    List.fold_left bindings ~init:(return []) ~f:(fun acc (pat, expr) ->
      let* acc_list = acc in
      let* anf_expr_body = anf_as_expr expr in
      match pat with
      | PatTuple (p1, p2, rest) ->
        let* tuple_var = fresh in
        let* component_bindings =
          build_tuple_top_level_bindings
            tuple_var
            (tuple_indexed_patterns p1 p2 rest)
        in
        let one_value (id, e) = AnfValue (NonRec, to_fun_bind (id, e), []) in
        let new_items =
          AnfValue (rec_flag, to_fun_bind (tuple_var, anf_expr_body), [])
          :: List.map component_bindings ~f:one_value
        in
        return (acc_list @ new_items)
      | _ ->
        let* var = get_var pat in
        return (acc_list @ [ AnfValue (rec_flag, to_fun_bind (var, anf_expr_body), []) ]))
;;

let anf_program (ast : program) : (anf_program, string) Result.t =
  let program' =
    List.fold_left ast ~init:(return []) ~f:(fun acc item ->
      let* acc_list = acc in
      let* item_anf = anf_structure_item item in
      return (acc_list @ item_anf))
  in
  run program'
;;
