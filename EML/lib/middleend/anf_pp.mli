(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val pp_anf_program : Format.formatter -> Anf.anf_program -> unit
val anf_to_string : Anf.anf_program -> string
