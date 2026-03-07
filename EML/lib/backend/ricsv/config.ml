(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Primitives the generated code can call. *)

type primitive =
  { name : string
  ; arity : int
  }

let primitive_arities ~enable_gc : primitive list =
  let base =
    [ { name = "print_int"; arity = 1 }
    ; { name = "print_endline"; arity = 1 }
    ; { name = "create_tuple"; arity = 2 }
    ; { name = "field"; arity = 2 }
    ; { name = "alloc_closure"; arity = 2 }
    ; { name = "eml_applyN"; arity = 3 }
    ]
  in
  if enable_gc
  then
    base
    @ [ { name = "get_heap_start"; arity = 0 }
      ; { name = "get_heap_final"; arity = 0 }
      ; { name = "collect"; arity = 0 }
      ; { name = "print_gc_status"; arity = 0 }
      ]
  else base
;;
