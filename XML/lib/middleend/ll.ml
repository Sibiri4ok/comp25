(** Copyright 2024, Mikhail Gavrilenko,
    Danila Rudnev-Stepanyan*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Common.Ast
open Anf
module SSet = Set.Make (String)
module SMap = Map.Make (String)

type supply = int

let fresh_name (base : ident) (n : supply) : ident * supply =
  base ^ "__ll$" ^ string_of_int n, n + 1
;;

let fv_im = function
  | Imm_num _ -> SSet.empty
  | Imm_ident x -> SSet.singleton x
;;

let rec fv_comp = function
  | Comp_imm i -> fv_im i
  | Comp_binop (_op, a, b) -> SSet.union (fv_im a) (fv_im b)
  | Comp_app (f, args) ->
    List.fold_left (fun s a -> SSet.union s (fv_im a)) (fv_im f) args
  | Comp_branch (c, t, e) -> SSet.union (fv_im c) (SSet.union (fv_anf t) (fv_anf e))
  | Comp_func (ps, body) ->
    let fvb = fv_anf body in
    List.fold_left (fun s p -> SSet.remove p s) fvb ps
  | Comp_tuple is | Comp_alloc is ->
    List.fold_left (fun s a -> SSet.union s (fv_im a)) SSet.empty is
  | Comp_load (addr, _off) -> fv_im addr

and fv_anf = function
  | Anf_comp_expr ce -> fv_comp ce
  | Anf_let (_rf, x, ce, body) -> SSet.union (fv_comp ce) (SSet.remove x (fv_anf body))
;;

let occurs_im x = function
  | Imm_ident y -> String.equal x y
  | _ -> false
;;

let rec escapes_comp x = function
  | Comp_imm i -> occurs_im x i
  | Comp_binop (_op, a, b) -> occurs_im x a || occurs_im x b
  | Comp_app (f, args) ->
    List.exists (occurs_im x) args
    ||
      (match f with
      | Imm_ident y when String.equal y x -> false
      | _ -> occurs_im x f)
  | Comp_branch (c, t, e) -> occurs_im x c || escapes_anf x t || escapes_anf x e
  | Comp_func (_ps, body) -> SSet.mem x (fv_anf body)
  | Comp_tuple is | Comp_alloc is -> List.exists (occurs_im x) is
  | Comp_load (addr, _off) -> occurs_im x addr

and escapes_anf x = function
  | Anf_comp_expr ce -> escapes_comp x ce
  | Anf_let (_rf, _y, ce, body) -> escapes_comp x ce || escapes_anf x body
;;

type ctx = (ident * ident list) SMap.t

let rewrite_app (env : ctx) (f : im_expr) (args : im_expr list) : im_expr * im_expr list =
  match f with
  | Imm_ident x ->
    (match SMap.find_opt x env with
     | None -> f, args
     | Some (lf, fvs) ->
       let fv_atoms = List.map (fun v -> Imm_ident v) fvs in
       Imm_ident lf, fv_atoms @ args)
  | _ -> f, args
;;

let rec lift_anf (env : ctx) (n : supply) (e : anf_expr)
  : (anf_expr * astructure_item list) * supply
  =
  match e with
  | Anf_comp_expr ce ->
    let (ce', defs), n' = lift_comp env n ce in
    (Anf_comp_expr ce', defs), n'
  | Anf_let (rf, x, ce, body) ->
    (match ce with
     | Comp_func (ps, fbody) ->
       if escapes_anf x body || escapes_anf x fbody
       then (
         let (fbody', d1), n1 = lift_anf env n fbody in
         let (body', d2), n2 = lift_anf env n1 body in
         (Anf_let (rf, x, Comp_func (ps, fbody'), body'), d1 @ d2), n2)
       else (
         let fvs =
           let all = fv_anf fbody in
           let all = List.fold_left (fun s p -> SSet.remove p s) all ps in
           let all = SSet.remove x all in
           SSet.elements all
         in
         let lifted_name, n1 = fresh_name x n in
         let env_body = SMap.add x (lifted_name, fvs) env in
         let (fbody', defs_body), n2 = lift_anf env_body n1 fbody in
         let def_item =
           Anf_str_value
             (Nonrecursive, lifted_name, Anf_comp_expr (Comp_func (fvs @ ps, fbody')))
         in
         let (body', defs_e2), n3 = lift_anf env_body n2 body in
         (body', defs_body @ (def_item :: defs_e2)), n3)
     | Comp_imm (Imm_ident y) ->
       (match SMap.find_opt y env with
        | Some (lf, fvs) ->
          let env' = SMap.add x (lf, fvs) env in
          lift_anf env' n body
        | None ->
          let (body', d2), n' = lift_anf env n body in
          (Anf_let (rf, x, ce, body'), d2), n')
     | Comp_app (Imm_ident lf_id, args) ->
       let args_are vs =
         try
           List.for_all2
             (fun v -> function
                | Imm_ident y -> String.equal v y
                | _ -> false)
             vs
             args
         with
         | Invalid_argument _ -> false
       in
       let hit =
         SMap.fold
           (fun _ (lf, fvs) acc -> acc || (String.equal lf lf_id && args_are fvs))
           env
           false
       in
       if hit
       then (
         let lf_opt =
           SMap.fold
             (fun _ (lf, fvs) acc ->
                if acc = None && String.equal lf lf_id && args_are fvs
                then Some lf
                else acc)
             env
             None
         in
         let env' =
           match lf_opt with
           | Some lf -> SMap.add x (lf, []) env
           | None -> env
         in
         lift_anf env' n body)
       else (
         let (ce', d1), n1 = lift_comp env n ce in
         let (body', d2), n2 = lift_anf env n1 body in
         (Anf_let (rf, x, ce', body'), d1 @ d2), n2)
     | _ ->
       let (ce', d1), n1 = lift_comp env n ce in
       let (body', d2), n2 = lift_anf env n1 body in
       (Anf_let (rf, x, ce', body'), d1 @ d2), n2)

and lift_comp (env : ctx) (n : supply) (ce : comp_expr)
  : (comp_expr * astructure_item list) * supply
  =
  match ce with
  | Comp_imm _ | Comp_binop _ | Comp_tuple _ | Comp_alloc _ | Comp_load _ -> (ce, []), n
  | Comp_app (f, args) ->
    let f', args' = rewrite_app env f args in
    (Comp_app (f', args'), []), n
  | Comp_branch (c, t, e) ->
    let (t', dt), n1 = lift_anf env n t in
    let (e', de), n2 = lift_anf env n1 e in
    (Comp_branch (c, t', e'), dt @ de), n2
  | Comp_func (ps, body) ->
    let (body', defs), n' = lift_anf env n body in
    (Comp_func (ps, body'), defs), n'
;;

let rec desugar_then_lift (env : ctx) (n : supply) (e : anf_expr)
  : (anf_expr * astructure_item list) * supply
  =
  match e with
  | Anf_let
      (Nonrecursive, tmp, Comp_func (ps, fbody), Anf_comp_expr (Comp_imm (Imm_ident tmp')))
    when String.equal tmp tmp' ->
    let (fbody', d), n' = lift_anf env n fbody in
    (Anf_comp_expr (Comp_func (ps, fbody')), d), n'
  | Anf_let
      ( Nonrecursive
      , tmp
      , Comp_func (ps, fbody)
      , Anf_let (_rf2, x, Comp_imm (Imm_ident tmp'), body) )
    when String.equal tmp tmp' ->
    if escapes_anf x body || escapes_anf x fbody
    then (
      let (e', d), n' = lift_anf env n e in
      (e', d), n')
    else (
      let fvs =
        let all = fv_anf fbody in
        let all = List.fold_left (fun s p -> SSet.remove p s) all ps in
        SSet.elements all
      in
      let lifted_name, n1 = fresh_name x n in
      let env' =
        env |> SMap.add x (lifted_name, fvs) |> SMap.add tmp (lifted_name, fvs)
      in
      let (fbody', d1), n2 = lift_anf env' n1 fbody in
      let def_item =
        Anf_str_value
          (Nonrecursive, lifted_name, Anf_comp_expr (Comp_func (fvs @ ps, fbody')))
      in
      let (body', d2), n3 = lift_anf env' n2 body in
      (body', d1 @ (def_item :: d2)), n3)
  | Anf_let (rf, x, ce, body) ->
    let (ce', d1), n1 = lift_comp env n ce in
    let (body', d2), n2 = desugar_then_lift env n1 body in
    (Anf_let (rf, x, ce', body'), d1 @ d2), n2
  | Anf_comp_expr ce ->
    let (ce', d), n' = lift_comp env n ce in
    (Anf_comp_expr ce', d), n'
;;

let lift_item (it : astructure_item) (n : supply)
  : (astructure_item * astructure_item list) * supply
  =
  match it with
  | Anf_str_eval e ->
    let (e1, d1), n1 = desugar_then_lift SMap.empty n e in
    let (e2, d2), n2 = lift_anf SMap.empty n1 e1 in
    (Anf_str_eval e2, d1 @ d2), n2
  | Anf_str_value (rf, name, e) ->
    let (e1, d1), n1 = desugar_then_lift SMap.empty n e in
    let (e2, d2), n2 = lift_anf SMap.empty n1 e1 in
    (Anf_str_value (rf, name, e2), d1 @ d2), n2
;;

let lambda_lift_program (p : aprogram) : aprogram =
  let (items_rev, defs), _ =
    List.fold_left
      (fun ((acc, dacc), n) it ->
         let (it', d), n' = lift_item it n in
         (it' :: acc, dacc @ d), n')
      (([], []), 1)
      p
  in
  List.rev items_rev @ defs
;;
