(** Copyright 2025-2026, Rodion Suvorov, Dmitriy Chirkov*)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast.Expression
open Ast

type immexpr =
  | ImmNum of int
  | ImmId of ident
[@@deriving eq, show { with_path = false }]

type binop =
  | Add
  | Sub
  | Mul
  | Le
  | Lt
  | Eq
  | Neq
[@@deriving eq, show { with_path = false }]

type cexpr =
  | CImm of immexpr
  | CBinop of binop * immexpr * immexpr
  | CApp of immexpr * immexpr list
  | CIte of immexpr * aexpr * aexpr
  | CFun of ident * aexpr
[@@deriving eq, show { with_path = false }]

and aexpr =
  | ACE of cexpr
  | ALet of rec_flag * ident * cexpr * aexpr
[@@deriving eq, show { with_path = false }]

type astructure_item =
  | AStr_value of rec_flag * ident * aexpr
  | AStr_eval of aexpr
[@@deriving eq, show { with_path = false }]

type aprogram = astructure_item list [@@deriving eq, show { with_path = false }]
