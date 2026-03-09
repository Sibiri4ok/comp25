(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error = LambdaWithoutParameters

val pp_error : Format.formatter -> error -> unit

val closure_conversion_result
  :  Frontend.Ast.program
  -> (Frontend.Ast.program, error) Result.t
