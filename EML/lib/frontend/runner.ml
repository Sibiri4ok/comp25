(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Format

type error = Parse of string

let pp_error ppf = function
  | Parse s -> fprintf ppf "Parse error: %s" s
;;

let parse (text : string) : (program, string) Result.t = Parser.parse text

let run (text : string) : (program, error) Result.t =
  match Parser.parse text with
  | Error s -> Error (Parse s)
  | Ok ast -> Ok ast
;;
