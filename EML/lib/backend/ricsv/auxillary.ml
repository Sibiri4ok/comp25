(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Architecture
open Riscv_backend
open Generator_state

let is_caller_saved = function
  | A _ | T _ -> true
  | Zero | RA | SP | S _ -> false
;;

let to_tagged_bool dst = add dst dst dst @ add_tag_items dst 1

let compare_ordering dst left_reg right_reg ~invert =
  let base = slt dst left_reg right_reg in
  (if invert then base @ xori dst dst 1 else base) @ to_tagged_bool dst
;;

let compare_eq_ne dst left_reg right_reg ~is_eq =
  let base = xor dst left_reg right_reg in
  (if is_eq then base @ seqz dst dst else base @ snez dst dst) @ to_tagged_bool dst
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

let bin_oper_to_string = Utils.Pretty_printer.string_of_bin_op

let vars_in_caller_saved_regs environment =
  Base.Map.to_alist environment
  |> List.filter_map (fun (variable_name, variable_location) ->
    match variable_location with
    | Loc_reg register when is_caller_saved register -> Some (variable_name, register)
    | _ -> None)
;;

let indices_of_args_to_spill generation_state immediate_arguments =
  let argument_overwrites_result_register = function
    | ImmediateConst _ -> false
    | ImmediateVar function_name -> Base.Map.mem generation_state.arity_map function_name
  in
  Base.List.foldi
    immediate_arguments
    ~init:[]
    ~f:(fun argument_index spilled_indices immediate_argument ->
      if argument_overwrites_result_register immediate_argument
      then argument_index :: spilled_indices
      else spilled_indices)
  |> List.rev
;;

type call_style =
  | Nullary of string
  | CurryChain of
      { function_name : string
      ; arity : int
      ; initial_arguments : immediate list
      ; remaining_arguments : immediate list
      }
  | Direct of
      { function_name : string
      ; arguments : immediate list
      }
  | ViaApplyNargs of
      { function_name : string
      ; argument_count : int
      ; arguments : immediate list
      }

let classify_call ~argument_count ~callee_arity_opt ~function_name ~arguments : call_style
  =
  match callee_arity_opt with
  | Some 0 when argument_count = 1 -> Nullary function_name
  | Some arity when argument_count > arity ->
    CurryChain
      { function_name
      ; arity
      ; initial_arguments = Base.List.take arguments arity
      ; remaining_arguments = Base.List.drop arguments arity
      }
  | Some arity when argument_count = arity -> Direct { function_name; arguments }
  | _ -> ViaApplyNargs { function_name; argument_count; arguments }
;;
