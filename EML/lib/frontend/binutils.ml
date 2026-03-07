(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Built-in names and arities. Single place to change stdlib/runtime for
    closure conversion, backend, etc. *)

type primitive =
  { name : string
  ; arity : int
  }

let all_runtime_prims : primitive list =
  [ { name = "print_int"; arity = 1 }; { name = "print_endline"; arity = 1 } ]
;;

let builtin_global_names =
  List.map (fun p -> p.name) all_runtime_prims @ Ast.unary_op_list @ Ast.bin_op_list
;;
