(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Middleend.Anf

type function_layout =
  { func_name : string
  ; asm_name : string
  ; params : immediate list
  ; body : anf_expr
  ; is_rec : bool
  }

type analysis_result =
  { functions : function_layout list
  ; resolve : int -> string -> (string * int) option
  }

val analyze : anf_program -> analysis_result
