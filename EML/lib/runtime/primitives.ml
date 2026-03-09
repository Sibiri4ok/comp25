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

let predefined_runtime_funcs : runtime_func_sig list =
  [ { name = "eml_applyN"; ret = RPtr; args = [ Ptr; Int; Ptr ] }
  ; { name = "create_tuple"; ret = RPtr; args = [ Int; Ptr ] }
  ; { name = "alloc_closure"; ret = RPtr; args = [ Ptr; Int ] }
  ; { name = "field"; ret = RPtr; args = [ Ptr; Int ] }
  ; { name = "llvm_call_indirect"; ret = RPtr; args = [ Ptr; Ptr; Int ] }
  ; { name = "print_int"; ret = RVoid; args = [ Int ] }
  ; { name = "init_gc"; ret = RVoid; args = [] }
  ; { name = "destroy_gc"; ret = RVoid; args = [] }
  ; { name = "set_ptr_stack"; ret = RVoid; args = [ Ptr ] }
  ; { name = "get_heap_start"; ret = RInt; args = [] }
  ; { name = "get_heap_final"; ret = RInt; args = [] }
  ; { name = "collect"; ret = RPtr; args = [] }
  ; { name = "print_gc_status"; ret = RPtr; args = [] }
  ; { name = "llvm.frameaddress.p0"; ret = RPtr; args = [ I32 ] }
  ]
;;

let runtime_primitive_arities : (string * int) list =
  List.map (fun { name; args; _ } -> name, List.length args) predefined_runtime_funcs
;;

let is_reserved (name : string) : bool =
  List.exists (fun { name = n; _ } -> String.equal n name) predefined_runtime_funcs
;;
