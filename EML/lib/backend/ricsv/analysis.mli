(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Phase 1: pure analysis of ANF program. No emission, no backend. *)

open Base
open Middleend.Anf

type function_layout =
  { f_id : string
  ; params : immediate list
  ; body : anf_expr
  ; slots_count : int
  }

type analysis_result =
  { arity_map : (string, int, String.comparator_witness) Map.t
  ; functions : function_layout list
  }

(** One pass over [program]: build arity map and per-function layout (params, body, slot count). *)
val analyze : anf_program -> analysis_result
