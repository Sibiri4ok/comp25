(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val gen_program
  :  output_file:string
  -> enable_gc:bool
  -> Middleend.Anf.anf_program
  -> (unit, string) Result.t
