(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Anf_types

type anf_state = { temps : int }

val anf_transform : Ast.program -> (astructure_item list, string) result
