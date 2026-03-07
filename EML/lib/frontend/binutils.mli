(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type primitive =
  { name : string
  ; arity : int
  }

val all_runtime_prims : primitive list
val builtin_global_names : string list
