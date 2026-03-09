(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type llvm_arg =
  | Ptr
  | Int
  | I32

type llvm_ret =
  | RPtr
  | RInt
  | RVoid

type runtime_func_sig =
  { name : string
  ; ret : llvm_ret
  ; args : llvm_arg list
  }

val predefined_runtime_funcs : runtime_func_sig list
val runtime_primitive_arities : (string * int) list
val is_reserved : string -> bool
