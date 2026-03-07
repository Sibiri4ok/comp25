(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Inferencer

type error =
  | Parse of string
  | Infer of Inferencer.error

val pp_error : Format.formatter -> error -> unit
val parse : string -> (program, string) Result.t

val run
  :  string
  -> TypeEnv.t
  -> (program * TypeEnv.t * (ident option * ty) list, error) Result.t
