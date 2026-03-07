(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Architecture
open Riscv_backend
open Generator_state
open Frontend.Ast

let is_caller_saved = function
  | A _ | T _ -> true
  | Zero | RA | SP | S _ -> false
;;

let to_tagged_bool dst = add dst dst dst @ add_tag_items dst 1

let compare_ordering dst r1 r2 ~invert =
  let base = slt dst r1 r2 in
  let tagged = if invert then base @ xori dst dst 1 else base in
  tagged @ to_tagged_bool dst
;;

let compare_eq_ne dst r1 r2 ~is_eq =
  let base = xor dst r1 r2 in
  let tagged = if is_eq then base @ seqz dst dst else base @ snez dst dst in
  tagged @ to_tagged_bool dst
;;

let bin_op dst op r1 r2 : (instr list, string) result =
  match op with
  | "+" -> Ok (add dst r1 r2 @ add_tag_items dst (-1))
  | "-" -> Ok (sub dst r1 r2 @ add_tag_items dst 1)
  | "*" -> Ok (srli r1 r1 1 @ addi r2 r2 (-1) @ mul dst r1 r2 @ add_tag_items dst 1)
  | "/" ->
    Ok
      (srli r1 r1 1 @ srli r2 r2 1 @ div dst r1 r2 @ add dst dst dst @ add_tag_items dst 1)
  | "<" -> Ok (compare_ordering dst r1 r2 ~invert:false)
  | ">" -> Ok (compare_ordering dst r2 r1 ~invert:false)
  | "<=" -> Ok (compare_ordering dst r2 r1 ~invert:true)
  | ">=" -> Ok (compare_ordering dst r1 r2 ~invert:true)
  | "=" -> Ok (compare_eq_ne dst r1 r2 ~is_eq:true)
  | "<>" -> Ok (compare_eq_ne dst r1 r2 ~is_eq:false)
  | _ -> Error ("unsupported binary operator: " ^ op)
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
  Base.Map.to_alist env
  |> List.filter_map (fun (name, loc) ->
    match loc with
    | Loc_reg r when is_caller_saved r -> Some (name, r)
    | _ -> None)
;;

let indices_of_args_to_spill state exps =
  let is_rewrites_result_regs state = function
    | ImmediateConst _ -> false
    | ImmediateVar id -> Base.Map.mem state.arity_map id
  in
  List.rev
    (snd
       (List.fold_left
          (fun (i, acc) arg ->
             i + 1, if is_rewrites_result_regs state arg then i :: acc else acc)
          (0, [])
          exps))
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
      ; first_args = Base.List.take args arity
      ; rest_args = Base.List.drop args arity
      }
  | Some arity when nargs = arity -> Direct { fname; args }
  | _ -> Via_apply_nargs { fname; nargs; args }
;;
