(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Frontend.Ast

type immediate =
  | ImmediateConst of const
  | ImmediateVar of ident
[@@deriving show { with_path = false }]

type complex_expr =
  | ComplexImmediate of immediate
  | ComplexUnit
  | ComplexBinOper of bin_oper * immediate * immediate
  | ComplexUnarOper of unar_oper * immediate
  | ComplexTuple of immediate * immediate * immediate list
  | ComplexField of immediate * int
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

type arity = int
type anf_bind = ident * anf_expr [@@deriving show { with_path = false }]
type anf_fun_bind = ident * arity * anf_expr [@@deriving show { with_path = false }]

type anf_structure =
  | AnfEval of anf_expr
  | AnfValue of is_rec * anf_fun_bind * anf_fun_bind list
[@@deriving show { with_path = false }]

type anf_program = anf_structure list [@@deriving show { with_path = false }]

val anf_program : program -> (anf_program, string) Result.t
