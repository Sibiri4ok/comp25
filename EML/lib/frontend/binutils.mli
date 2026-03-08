(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type primitive =
  { name : string
  ; arity : int
  }

val primitive_arities : enable_gc:bool -> primitive list
val primitive_names : enable_gc:bool -> string list
