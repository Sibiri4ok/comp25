(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Base
open Frontend.Ast
open Architecture.Riscv_backend
open Middleend.Anf

let compare_ordering dst r1 r2 ~invert =
  let base = slt dst r1 r2 in
  if invert then base @ xori dst dst 1 else base
;;

let compare_eq_ne dst r1 r2 ~is_eq =
  let base = xor dst r1 r2 in
  if is_eq then base @ seqz dst dst else base @ snez dst dst
;;

let bin_op dst op r1 r2 =
  match op with
  | "+" -> add dst r1 r2 @ add_tag_items dst (-1)
  | "-" -> sub dst r1 r2 @ add_tag_items dst 1
  | "*" -> srli r1 r1 1 @ addi r2 r2 (-1) @ mul dst r1 r2 @ add_tag_items dst 1
  | "<" -> compare_ordering dst r1 r2 ~invert:false
  | ">" -> compare_ordering dst r2 r1 ~invert:false
  | "<=" -> compare_ordering dst r2 r1 ~invert:true
  | ">=" -> compare_ordering dst r1 r2 ~invert:true
  | "=" -> compare_eq_ne dst r1 r2 ~is_eq:true
  | "<>" -> compare_eq_ne dst r1 r2 ~is_eq:false
  | _ -> failwith ("unsupported binary operator: " ^ op)
;;

let bin_oper_to_string = function
  | Plus -> "+"
  | Minus -> "-"
  | Multiply -> "*"
  | Division -> "/"
  | And -> "&&"
  | Or -> "||"
  | Equal -> "="
  | NotEqual -> "<>"
  | GreaterThan -> ">"
  | LowerThan -> "<"
  | GretestEqual -> ">="
  | LowestEqual -> "<="
;;

let vars_in_caller_saved_regs env =
  Map.to_alist env
  |> List.filter_map ~f:(fun (name, loc) ->
    match loc with
    | Loc_reg r when is_caller_saved r -> Some (name, r)
    | _ -> None)
;;

type call_style =
  | Nullary of string
  | Curry_chain of
      { fname : string
      ; arity : int
      ; first_args : immediate list
      ; rest_args : immediate list
      }
  | Direct of
      { fname : string
      ; args : immediate list
      }
  | Via_apply_nargs of
      { fname : string
      ; nargs : int
      ; args : immediate list
      }

let classify_call ~nargs ~callee_arity_opt ~fname ~args : call_style =
  match callee_arity_opt with
  | Some 0 when nargs = 1 -> Nullary fname
  | Some arity when nargs > arity ->
    Curry_chain
      { fname
      ; arity
      ; first_args = List.take args arity
      ; rest_args = List.drop args arity
      }
  | Some arity when nargs = arity -> Direct { fname; args }
  | _ -> Via_apply_nargs { fname; nargs; args }
;;
