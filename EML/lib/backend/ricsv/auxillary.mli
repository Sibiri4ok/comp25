(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf
open Architecture
open Riscv_backend

val is_caller_saved : reg -> bool
val bin_op : reg -> string -> reg -> reg -> (instr list, string) result
val bin_oper_to_string : Frontend.Ast.bin_oper -> string

val vars_in_caller_saved_regs
  :  (string, location, Base.String.comparator_witness) Base.Map.t
  -> (string * reg) list

val indices_of_args_to_spill : Generator_state.state -> immediate list -> int list

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

val classify_call
  :  argument_count:int
  -> callee_arity_opt:int option
  -> function_name:string
  -> arguments:immediate list
  -> call_style
