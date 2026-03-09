(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast

val string_of_bin_op : bin_oper -> string
val string_of_unary_op : unar_oper -> string
val pp_bin_op : Format.formatter -> bin_oper -> unit
val pp_unary_op : Format.formatter -> unar_oper -> unit
val pp_const : Format.formatter -> const -> unit
val pp_pattern : Format.formatter -> pattern -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_structure_item : Format.formatter -> structure -> unit
val pp_structure : Format.formatter -> structure list -> unit
