[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Gleb Nasretdinov, Ilhom Kombaev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Runs a QuickCheck tests. 
 @param n Number of generated Ast.
 @param printer Printer that formats generated Ast on a failure case.
 @param dparse Don't check anything, only generate Ast and print it. *)
val run : int -> (Format.formatter -> Ast.program -> unit) -> bool -> unit
