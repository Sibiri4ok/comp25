(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast
open Inferencer
open Anf

type error =
  | Infer of Inferencer.error
  | Closure of Cc.error
  | Lifting of Ll.error
  | Anf of string

val pp_error : Format.formatter -> error -> unit
val run : program -> TypeEnv.t -> (anf_program * TypeEnv.t, error) Result.t
