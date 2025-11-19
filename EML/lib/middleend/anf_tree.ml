(** Copyright 2024-2025, Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend
open Ast
open Base

type immediate =
  | ImmediateConst of const
  | ImmediateVar of ident
[@@deriving show { with_path = false }]

type complex_expr =
  | ComplexImmediate of immediate
  | ComplexBinOper of bin_oper * immediate * immediate
  | ComplexUnarOper of unar_oper * immediate
  | ComplexTuple of immediate * immediate * immediate list
  | ComplexList of immediate list
  | ComplexOption of immediate option
  | ComplexApp of immediate * immediate * immediate list
  | ComplexLambda of pattern list * anf_expr
  | ComplexBranch of immediate * anf_expr * anf_expr
[@@deriving show { with_path = false }]

and anf_expr =
  | AnfLet of is_rec * ident * complex_expr * anf_expr
  | AnfExpr of complex_expr
[@@deriving show { with_path = false }]

type anf_bind = ident * anf_expr [@@deriving show { with_path = false }]

type anf_structure =
  | AnfEval of anf_expr
  | AnfValue of is_rec * anf_bind * anf_bind list
[@@deriving show { with_path = false }]

type anf_program = anf_structure list [@@deriving show { with_path = false }]
