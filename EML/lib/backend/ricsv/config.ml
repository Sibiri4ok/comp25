(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Primitives the generated code can call. *)

type prim = { name : string; arity : int }

let base_arities : prim list =
  [ { name = "print_int"; arity = 1 }
  ; { name = "print_endline"; arity = 1 }
  ; { name = "alloc_closure"; arity = 2 }
  ; { name = "eml_applyN"; arity = 3 }
  ]
;;
