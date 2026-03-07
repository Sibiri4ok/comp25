(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast
open Anf

type error =
  | Closure of Cc.error
  | Lifting of Ll.error
  | Anf of string

val pp_error : Format.formatter -> error -> unit
val run : program -> (anf_program, error) Result.t
