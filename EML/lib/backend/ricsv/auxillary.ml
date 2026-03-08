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

let compare_ordering dst left_reg right_reg ~invert =
  let base = slt dst left_reg right_reg in
  let tagged = if invert then base @ xori dst dst 1 else base in
  tagged @ to_tagged_bool dst
;;

let compare_eq_ne dst left_reg right_reg ~is_eq =
  let base = xor dst left_reg right_reg in
  let tagged = if is_eq then base @ seqz dst dst else base @ snez dst dst in
  tagged @ to_tagged_bool dst
;;

let bin_op dst op left_reg right_reg : (instr list, string) result =
  match op with
  | "+" -> Ok (add dst left_reg right_reg @ add_tag_items dst (-1))
  | "-" -> Ok (sub dst left_reg right_reg @ add_tag_items dst 1)
  | "*" ->
    Ok
      (srli left_reg left_reg 1
       @ addi right_reg right_reg (-1)
       @ mul dst left_reg right_reg
       @ add_tag_items dst 1)
  | "/" ->
    Ok
      (srli left_reg left_reg 1
       @ srli right_reg right_reg 1
       @ div dst left_reg right_reg
       @ add dst dst dst
       @ add_tag_items dst 1)
  | "<" -> Ok (compare_ordering dst left_reg right_reg ~invert:false)
  | ">" -> Ok (compare_ordering dst right_reg left_reg ~invert:false)
  | "<=" -> Ok (compare_ordering dst right_reg left_reg ~invert:true)
  | ">=" -> Ok (compare_ordering dst left_reg right_reg ~invert:true)
  | "=" -> Ok (compare_eq_ne dst left_reg right_reg ~is_eq:true)
  | "<>" -> Ok (compare_eq_ne dst left_reg right_reg ~is_eq:false)
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
          (fun (index, dangerous_indices) arg ->
             ( index + 1
             , if is_rewrites_result_regs state arg
               then index :: dangerous_indices
               else dangerous_indices ))
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
