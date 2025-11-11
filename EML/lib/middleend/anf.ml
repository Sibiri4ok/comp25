(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend

open Ast
open Base

module ANFMonad = struct
  type 'a t = int -> int * ('a, string) Result.t

  let return x = fun counter -> (counter, Ok x)
  
  let ( >>= ) m f = fun counter ->
    match m counter with
    | (counter', Ok a) -> f a counter'
    | (counter', Error e) -> (counter', Error e)

  let fresh : string t = fun counter ->
    (counter + 1, Ok ("anf_t" ^ Int.to_string counter))

  let run m = m 0 |> snd

  let fail msg = fun counter -> (counter, Error msg)

  (** Монадические операторы *)
  module Syntax = struct
    let ( let* ) = ( >>= )
  end
end

open ANFMonad
open ANFMonad.Syntax

(** ANF представления *)
type imm =
  | ImmConst of const
  | ImmVar of ident
[@@deriving show { with_path = false }]

type cexpr =
  | CImm of imm
  | CBinop of bin_oper * imm * imm
  | CUnarOper of unar_oper * imm
  | CTuple of imm * imm * imm list
  | CList of imm list
  | COption of imm option
  | CApp of imm * imm * imm list
  | CLambda of pattern list * aexpr
  | CIte of imm * aexpr * aexpr
[@@deriving show { with_path = false }]

and aexpr =
  | ALet of is_rec * ident * cexpr * aexpr
  | ACExpr of cexpr
[@@deriving show { with_path = false }]

type abinding = ident * aexpr [@@deriving show { with_path = false }]
type astr_item = 
  | AEval of aexpr
  | AValue of is_rec * abinding * abinding list
[@@deriving show { with_path = false }]

type aprogram = astr_item list [@@deriving show { with_path = false }]

(** Вспомогательные функции *)
let mk_alet rf name1 v body =
  match rf, body with
  | NonRec, ACExpr (CImm (ImmVar name2)) when String.equal name1 name2 -> ACExpr v
  | _ -> ALet (rf, name1, v, body)
;;

let is_simple_pattern = function
  | PatVariable _ | PatAny | PatUnit -> true
  | _ -> false
;;

let pattern_to_ident = function
  | PatVariable x -> Some x
  | _ -> None
;;

(** Основные функции преобразования *)
let rec anf (e : expr) (k : imm -> aexpr t) : aexpr t =
  match e with
  | ExpConst c -> k (ImmConst c)
  | ExpIdent x -> k (ImmVar x)
  
  | ExpUnarOper (op, e) ->
      anf_as_imm e (fun imm ->
        let* temp = fresh in
        let* ehole = k (ImmVar temp) in
        return (ALet (NonRec, temp, CUnarOper (op, imm), ehole)))
  
  | ExpBinOper (op, e1, e2) ->
      anf_as_imm e1 (fun imm1 ->
        anf_as_imm e2 (fun imm2 ->
          let* temp = fresh in
          let* ehole = k (ImmVar temp) in
          return (ALet (NonRec, temp, CBinop (op, imm1, imm2), ehole))))
  
  | ExpTuple (e1, e2, rest) ->
      let all_exprs = e1 :: e2 :: rest in
      anf_list all_exprs (fun imms ->
        match imms with
        | i1 :: i2 :: rest_imm ->
            let* temp = fresh in
            let* ehole = k (ImmVar temp) in
            return (ALet (NonRec, temp, CTuple (i1, i2, rest_imm), ehole))
        | _ -> fail "Invalid tuple")
  
  | ExpList exprs ->
      anf_list exprs (fun imms ->
        let* temp = fresh in
        let* ehole = k (ImmVar temp) in
        return (ALet (NonRec, temp, CList imms, ehole)))
  
  | ExpOption opt_expr ->
      (match opt_expr with
       | None -> 
           let* temp = fresh in
           let* ehole = k (ImmVar temp) in
           return (ALet (NonRec, temp, COption None, ehole))
       | Some expr ->
           anf_as_imm expr (fun imm ->
             let* temp = fresh in
             let* ehole = k (ImmVar temp) in
             return (ALet (NonRec, temp, COption (Some imm), ehole))))
  
  | ExpBranch (cond, then_expr, else_expr) ->
      anf_as_imm cond (fun imm_cond ->
        let* then_anf = anf then_expr (fun imm -> return (ACExpr (CImm imm))) in
        let* else_anf = 
          match else_expr with
          | Some e -> anf e (fun imm -> return (ACExpr (CImm imm)))
          | None -> return (ACExpr (CImm (ImmConst (ConstBool false))))
        in
        let* temp = fresh in
        let* ehole = k (ImmVar temp) in
        return (ALet (NonRec, temp, CIte (imm_cond, then_anf, else_anf), ehole)))
  
  | ExpLet (rec_flag, (pat, e1), _, e2) ->
      if is_simple_pattern pat then
        let* e1_anf = anf e1 (fun imm -> return (ACExpr (CImm imm))) in
        let* body_anf = anf e2 k in
        (match pattern_to_ident pat with
         | Some name ->
             let* cexpr_body = 
               match e1_anf with
               | ACExpr c -> return c
               | _ -> fail "Expected cexpr"
             in
             return (ALet (rec_flag, name, cexpr_body, body_anf))
         | None -> 
             let* temp = fresh in
             let* cexpr_body = 
               match e1_anf with
               | ACExpr c -> return c
               | _ -> fail "Expected cexpr"
             in
             return (ALet (NonRec, temp, cexpr_body, body_anf)))
      else
        fail "Complex patterns in let bindings not yet supported"
  
  | ExpLambda (patterns, body) ->
      let* body_anf = anf body (fun imm -> return (ACExpr (CImm imm))) in
      let* temp = fresh in
      let* ehole = k (ImmVar temp) in
      return (ALet (NonRec, temp, CLambda (patterns, body_anf), ehole))
  
  | ExpFunction (func, arg) ->
      anf_as_imm func (fun imm_func ->
        anf_as_imm arg (fun imm_arg ->
          let* temp = fresh in
          let* ehole = k (ImmVar temp) in
          return (ALet (NonRec, temp, CApp (imm_func, imm_arg, []), ehole))))
  
  | ExpTypeAnnotation (e, _) ->
      (* Игнорируем аннотации типов в ANF *)
      anf e k
  

and anf_as_imm (e : expr) (k : imm -> aexpr t) : aexpr t =
  match e with
  | ExpConst _ | ExpIdent _ -> anf e k
  | ExpLambda _ -> anf e k
  | _ ->
      anf e (fun imm -> k imm)

and anf_list (exprs : expr list) (k : imm list -> aexpr t) : aexpr t =
  match exprs with
  | [] -> k []
  | hd :: tl ->
      anf_as_imm hd (fun imm_hd ->
        anf_list tl (fun imm_tl -> k (imm_hd :: imm_tl)))

(** Преобразование структуры *)
let anf_structure_item (item : structure) : astr_item t =
  match item with
  | SEval expr ->
      let* result = anf expr (fun imm -> return (ACExpr (CImm imm))) in
      return (AEval result)
  
  | SValue (rec_flag, (pat, expr), _) ->
      if is_simple_pattern pat then
        let* aexpr_body = anf expr (fun imm -> return (ACExpr (CImm imm))) in
        match pattern_to_ident pat with
        | Some name ->
            return (AValue (rec_flag, (name, aexpr_body), []))
        | None ->
            return (AValue (rec_flag, ("_", aexpr_body), []))
      else
        fail "Complex patterns in top-level bindings not yet supported"

(** Преобразование всей программы *)
let anf_program (program : program) : (aprogram, string) Result.t =
  let program' =
    List.fold_right program ~init:(return []) ~f:(fun item acc ->
      let* acc_list = acc in
      let* item_anf = anf_structure_item item in
      return (item_anf :: acc_list))
  in
  run program'