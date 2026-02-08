(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open EML_lib.Frontend.Parser
open EML_lib.Frontend.Ast
open Printf

let parse_program input =
  match parse input with
  | Ok ast -> printf "%s\n" (show_program ast)
  | Error fail -> printf "Ошибка: %s\n" fail
;;

let () = parse_program "let x = 1 + 2"
