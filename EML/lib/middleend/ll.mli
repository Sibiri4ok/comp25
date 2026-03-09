(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type error =
  | RecLetEmptyBinding
  | SValueEmptyBinding

val pp_error : Format.formatter -> error -> unit

val lambda_lifting_result
  :  Frontend.Ast.program
  -> (Frontend.Ast.structure list, error) Result.t
