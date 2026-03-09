(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val is_simple_pattern : Frontend.Ast.pattern -> bool
val is_tuple_pattern : Frontend.Ast.pattern -> bool
val extract_tuple_pattern_idents : string list -> Frontend.Ast.pattern -> string list
val pattern_to_ident : Frontend.Ast.pattern -> string option
