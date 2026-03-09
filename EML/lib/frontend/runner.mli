(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

type error = Parse of string

val pp_error : Format.formatter -> error -> unit
val parse : string -> (program, string) Result.t
val run : string -> (program, error) Result.t
