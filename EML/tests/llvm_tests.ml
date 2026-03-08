(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** LLVM IR codegen tests (analogous to riscv_tests.ml). *)

open EML_lib
open Frontend.Parser
open Middleend.Anf

let compile_llvm src : string =
  match parse src with
  | Error e -> "Parse error: " ^ e
  | Ok ast ->
    (match anf_program ast with
     | Error e -> "ANF error: " ^ e
     | Ok anf ->
       let buf = Buffer.create 4096 in
       let ppf = Format.formatter_of_buffer buf in
       (match Backend.Llvm_ir.Runner.gen_program ~enable_gc:false ppf anf with
        | Ok () ->
          Format.pp_print_flush ppf ();
          Buffer.contents buf
        | Error e -> "Codegen error: " ^ e))
;;

let run_llvm src = Format.printf "%s" (compile_llvm src)

let%expect_test "unit_main" =
  run_llvm "let main = ()";
  [%expect
    {|
; ModuleID = 'EML'
source_filename = "EML"

declare ptr @eml_applyN(ptr, i64, ptr)

declare ptr @create_tuple(i64, ptr)

declare ptr @alloc_closure(ptr, i64)

declare ptr @field(ptr, i64)

declare ptr @llvm_call_indirect(ptr, ptr, i64)

declare void @print_int(i64)

declare void @init_gc()

declare void @destroy_gc()

declare void @set_ptr_stack(ptr)

declare i64 @get_heap_start()

declare i64 @get_heap_final()

declare ptr @collect()

declare ptr @print_gc_status()

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr @llvm.frameaddress.p0(i32 immarg) #0

define ptr @eml_main() {
entry:
  ret ptr inttoptr (i64 1 to ptr)
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }

|}]
;;

let%expect_test "int_main" =
  run_llvm "let main = 42";
  [%expect
    {|
; ModuleID = 'EML'
source_filename = "EML"

declare ptr @eml_applyN(ptr, i64, ptr)

declare ptr @create_tuple(i64, ptr)

declare ptr @alloc_closure(ptr, i64)

declare ptr @field(ptr, i64)

declare ptr @llvm_call_indirect(ptr, ptr, i64)

declare void @print_int(i64)

declare void @init_gc()

declare void @destroy_gc()

declare void @set_ptr_stack(ptr)

declare i64 @get_heap_start()

declare i64 @get_heap_final()

declare ptr @collect()

declare ptr @print_gc_status()

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr @llvm.frameaddress.p0(i32 immarg) #0

define ptr @eml_main() {
entry:
  ret ptr inttoptr (i64 85 to ptr)
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }

|}]
;;

let%expect_test "unary_minus" =
  run_llvm "let x = -5";
  [%expect
    {|
; ModuleID = 'EML'
source_filename = "EML"

declare ptr @eml_applyN(ptr, i64, ptr)

declare ptr @create_tuple(i64, ptr)

declare ptr @alloc_closure(ptr, i64)

declare ptr @field(ptr, i64)

declare ptr @llvm_call_indirect(ptr, ptr, i64)

declare void @print_int(i64)

declare void @init_gc()

declare void @destroy_gc()

declare void @set_ptr_stack(ptr)

declare i64 @get_heap_start()

declare i64 @get_heap_final()

declare ptr @collect()

declare ptr @print_gc_status()

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr @llvm.frameaddress.p0(i32 immarg) #0

define ptr @x() {
entry:
  ret ptr inttoptr (i64 -9 to ptr)
}

define ptr @eml_main() {
entry:
  ret ptr inttoptr (i64 1 to ptr)
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }

|}]
;;

let%expect_test "unary_not" =
  run_llvm "let x = not true";
  [%expect
    {|
; ModuleID = 'EML'
source_filename = "EML"

declare ptr @eml_applyN(ptr, i64, ptr)

declare ptr @create_tuple(i64, ptr)

declare ptr @alloc_closure(ptr, i64)

declare ptr @field(ptr, i64)

declare ptr @llvm_call_indirect(ptr, ptr, i64)

declare void @print_int(i64)

declare void @init_gc()

declare void @destroy_gc()

declare void @set_ptr_stack(ptr)

declare i64 @get_heap_start()

declare i64 @get_heap_final()

declare ptr @collect()

declare ptr @print_gc_status()

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr @llvm.frameaddress.p0(i32 immarg) #0

define ptr @x() {
entry:
  ret ptr inttoptr (i64 2 to ptr)
}

define ptr @eml_main() {
entry:
  ret ptr inttoptr (i64 1 to ptr)
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }

|}]
;;

let%expect_test "mul_only" =
  run_llvm "let main = 7 * 8";
  [%expect
    {|
; ModuleID = 'EML'
source_filename = "EML"

declare ptr @eml_applyN(ptr, i64, ptr)

declare ptr @create_tuple(i64, ptr)

declare ptr @alloc_closure(ptr, i64)

declare ptr @field(ptr, i64)

declare ptr @llvm_call_indirect(ptr, ptr, i64)

declare void @print_int(i64)

declare void @init_gc()

declare void @destroy_gc()

declare void @set_ptr_stack(ptr)

declare i64 @get_heap_start()

declare i64 @get_heap_final()

declare ptr @collect()

declare ptr @print_gc_status()

; Function Attrs: nocallback nofree nosync nounwind willreturn memory(none)
declare ptr @llvm.frameaddress.p0(i32 immarg) #0

define ptr @eml_main() {
entry:
  ret ptr inttoptr (i64 113 to ptr)
}

attributes #0 = { nocallback nofree nosync nounwind willreturn memory(none) }

|}]
;;
