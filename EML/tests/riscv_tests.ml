(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** RISC-V codegen tests. *)

open EML_lib
open Frontend.Parser
open Middleend.Anf

let compile src : string =
  match parse src with
  | Error e -> "Parse error: " ^ e
  | Ok ast ->
    (match anf_program ast with
     | Error e -> "ANF error: " ^ e
     | Ok anf ->
       let buf = Buffer.create 1024 in
       let ppf = Format.formatter_of_buffer buf in
       (match Backend.Ricsv.Runner.gen_program ppf anf with
        | Ok () ->
          Format.pp_print_flush ppf ();
          Buffer.contents buf
        | Error e -> "Codegen error: " ^ e))
;;

let run src = Format.printf "%s" (compile src)

let%expect_test "unary_minus" =
  run "let x = -5";
  [%expect
    {|
.section .text
  .globl x
  .type x, @function
x:
  mv t1, fp
  addi sp, sp, -16
  addi fp, sp, 0
  sd ra, 8(fp)
  sd t1, 0(fp)
  li t0, 11
  li a0, 1
  sub a0, a0, t0
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  ret

  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -16
  addi fp, sp, 0
  sd ra, 8(fp)
  sd t1, 0(fp)
  li a0, 1
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "unary_not" =
  run "let x = not true";
  [%expect
    {|
.section .text
  .globl x
  .type x, @function
x:
  mv t1, fp
  addi sp, sp, -16
  addi fp, sp, 0
  sd ra, 8(fp)
  sd t1, 0(fp)
  li t0, 3
  xori a0, t0, 3
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  ret

  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -16
  addi fp, sp, 0
  sd ra, 8(fp)
  sd t1, 0(fp)
  li a0, 1
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "unit_main" =
  run "let main = ()";
  [%expect
    {|
.section .text
  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -16
  addi fp, sp, 0
  sd ra, 8(fp)
  sd t1, 0(fp)
  li a0, 1
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "mul_only" =
  run "let main = 7 * 8";
  [%expect
    {|
.section .text
  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -16
  addi fp, sp, 0
  sd ra, 8(fp)
  sd t1, 0(fp)
  li t0, 15
  li t1, 17
  srli t0, t0, 1
  addi t1, t1, -1
  mul a0, t0, t1
  addi a0, a0, 1
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "double_fn" =
  run
    {|
  let double x = x + x
  let main = double 21
  |};
  [%expect
    {|
.section .text
  .globl double
  .type double, @function
double:
  mv t1, fp
  addi sp, sp, -16
  addi fp, sp, 0
  sd ra, 8(fp)
  sd t1, 0(fp)
  sd a0, -8(fp)
  ld t0, -8(fp)
  ld t1, -8(fp)
  add a0, t0, t1
  addi a0, a0, -1
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  ret

  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -200
  addi fp, sp, 184
  sd ra, 8(fp)
  sd t1, 0(fp)
  li a0, 43
  call double
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "abs_fn" =
  run
    {|
  let abs x = if x < 0 then -x else x
  let main = abs 7
  |};
  [%expect
    {|
.section .text
  .globl abs
  .type abs, @function
abs:
  mv t1, fp
  addi sp, sp, -32
  addi fp, sp, 16
  sd ra, 8(fp)
  sd t1, 0(fp)
  sd a0, -8(fp)
  ld t0, -8(fp)
  li t1, 1
  slt a0, t0, t1
  add a0, a0, a0
  addi a0, a0, 1
  sd a0, -16(fp)
  ld t0, -16(fp)
  li t1, 1
  beq t0, t1, else_0
  ld t0, -8(fp)
  li a0, 1
  sub a0, a0, t0
  j end_0
else_0:
  ld a0, -8(fp)
end_0:
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  ret

  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -200
  addi fp, sp, 184
  sd ra, 8(fp)
  sd t1, 0(fp)
  li a0, 15
  call abs
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "nested_calls" =
  run
    {|
  let sq x = x * x
  let sum_of_squares a b = sq a + sq b
  let main = sum_of_squares 3 4
  |};
  [%expect
    {|
.section .text
  .globl sq
  .type sq, @function
sq:
  mv t1, fp
  addi sp, sp, -16
  addi fp, sp, 0
  sd ra, 8(fp)
  sd t1, 0(fp)
  sd a0, -8(fp)
  ld t0, -8(fp)
  ld t1, -8(fp)
  srli t0, t0, 1
  addi t1, t1, -1
  mul a0, t0, t1
  addi a0, a0, 1
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  ret

  .globl sum_of_squares
  .type sum_of_squares, @function
sum_of_squares:
  mv t1, fp
  addi sp, sp, -400
  addi fp, sp, 384
  sd ra, 8(fp)
  sd t1, 0(fp)
  sd a0, -8(fp)
  sd a1, -16(fp)
  ld a0, -8(fp)
  call sq
  sd a0, -24(fp)
  ld a0, -16(fp)
  call sq
  sd a0, -32(fp)
  ld t0, -24(fp)
  ld t1, -32(fp)
  add a0, t0, t1
  addi a0, a0, -1
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  ret

  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -208
  addi fp, sp, 192
  sd ra, 8(fp)
  sd t1, 0(fp)
  li a0, 7
  li a1, 9
  call sum_of_squares
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "fibonacci" =
  run
    {|
  let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)
  let main = fib 6
  |};
  [%expect
    {|
.section .text
  .globl fib
  .type fib, @function
fib:
  mv t1, fp
  addi sp, sp, -432
  addi fp, sp, 416
  sd ra, 8(fp)
  sd t1, 0(fp)
  sd a0, -8(fp)
  ld t0, -8(fp)
  li t1, 5
  slt a0, t0, t1
  add a0, a0, a0
  addi a0, a0, 1
  sd a0, -16(fp)
  ld t0, -16(fp)
  li t1, 1
  beq t0, t1, else_0
  li a0, 3
  j end_0
else_0:
  ld t0, -8(fp)
  li t1, 3
  sub a0, t0, t1
  addi a0, a0, 1
  sd a0, -24(fp)
  ld a0, -24(fp)
  call fib
  sd a0, -32(fp)
  ld t0, -8(fp)
  li t1, 5
  sub a0, t0, t1
  addi a0, a0, 1
  sd a0, -40(fp)
  ld a0, -40(fp)
  call fib
  sd a0, -48(fp)
  ld t0, -32(fp)
  ld t1, -48(fp)
  add a0, t0, t1
  addi a0, a0, -1
end_0:
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  ret

  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -200
  addi fp, sp, 184
  sd ra, 8(fp)
  sd t1, 0(fp)
  li a0, 13
  call fib
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "is_positive" =
  run
    {|
  let is_positive n = n > 0
  let main = is_positive 42
  |};
  [%expect
    {|
.section .text
  .globl is_positive
  .type is_positive, @function
is_positive:
  mv t1, fp
  addi sp, sp, -16
  addi fp, sp, 0
  sd ra, 8(fp)
  sd t1, 0(fp)
  sd a0, -8(fp)
  ld t0, -8(fp)
  li t1, 1
  slt a0, t1, t0
  add a0, a0, a0
  addi a0, a0, 1
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  ret

  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -200
  addi fp, sp, 184
  sd ra, 8(fp)
  sd t1, 0(fp)
  li a0, 85
  call is_positive
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "mul3" =
  run
    {|
  let mul3 a b c = a * b * c
  let main = mul3 2 3 4
  |};
  [%expect
    {|
.section .text
  .globl mul3
  .type mul3, @function
mul3:
  mv t1, fp
  addi sp, sp, -24
  addi fp, sp, 8
  sd ra, 8(fp)
  sd t1, 0(fp)
  sd a0, -8(fp)
  sd a1, -16(fp)
  sd a2, -24(fp)
  ld t0, -8(fp)
  ld t1, -16(fp)
  srli t0, t0, 1
  addi t1, t1, -1
  mul a0, t0, t1
  addi a0, a0, 1
  sd a0, -32(fp)
  ld t0, -32(fp)
  ld t1, -24(fp)
  srli t0, t0, 1
  addi t1, t1, -1
  mul a0, t0, t1
  addi a0, a0, 1
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  ret

  .globl main
  .type main, @function
main:
  mv t1, fp
  addi sp, sp, -216
  addi fp, sp, 200
  sd ra, 8(fp)
  sd t1, 0(fp)
  li a0, 5
  li a1, 7
  li a2, 9
  call mul3
  addi sp, fp, 16
  ld ra, 8(fp)
  ld fp, 0(fp)
  li a0, 0
  ret
|}]
;;

let%expect_test "test1" =
  run
    {|
   let large x = if 0<>x then print_int 0 else print_int 1
   let main =
     let x = if (if (if 0
                     then 0 else (let t42 = print_int 42 in 1))
                 then 0 else 1)
             then 0 else 1 in
     large x
  |};
  [%expect
    {|
      .section .text
        .globl large
        .type large, @function
      large:
        mv t1, fp
        addi sp, sp, -400
        addi fp, sp, 384
        sd ra, 8(fp)
        sd t1, 0(fp)
        sd a0, -8(fp)
        li t0, 1
        ld t1, -8(fp)
        xor a0, t0, t1
        snez a0, a0
        add a0, a0, a0
        addi a0, a0, 1
        sd a0, -16(fp)
        ld t0, -16(fp)
        li t1, 1
        beq t0, t1, else_0
        li a0, 1
        call print_int
        j end_0
      else_0:
        li a0, 3
        call print_int
      end_0:
        addi sp, fp, 16
        ld ra, 8(fp)
        ld fp, 0(fp)
        ret

        .globl main
        .type main, @function
      main:
        mv t1, fp
        addi sp, sp, -440
        addi fp, sp, 424
        sd ra, 8(fp)
        sd t1, 0(fp)
        li t0, 1
        li t1, 1
        beq t0, t1, else_1
        li a0, 1
        j end_1
      else_1:
        li a0, 85
        call print_int
        sd a0, -8(fp)
        li a0, 3
      end_1:
        sd a0, -16(fp)
        ld t0, -16(fp)
        li t1, 1
        beq t0, t1, else_2
        li a0, 1
        j end_2
      else_2:
        li a0, 3
      end_2:
        sd a0, -24(fp)
        ld t0, -24(fp)
        li t1, 1
        beq t0, t1, else_3
        li a0, 1
        j end_3
      else_3:
        li a0, 3
      end_3:
        sd a0, -32(fp)
        ld a0, -32(fp)
        call large
        addi sp, fp, 16
        ld ra, 8(fp)
        ld fp, 0(fp)
        li a0, 0
        ret
|}]
;;


let%expect_test "codegen closure fn with 10 arg" =
  run
    {|
  let add a b c d e f g = a + b + c + d + e + f + g

  let main =
    let temp1 = add 1 1 1 1 in
    let temp2 = temp1 1 1 in
    let temp3 = temp2 1 1 in
    print_int temp3
  ;;
  |};
  [%expect
    {|
    .section .text
      .globl add
      .type add, @function
    add:
      mv t1, fp
      addi sp, sp, -56
      addi fp, sp, 40
      sd ra, 8(fp)
      sd t1, 0(fp)
      sd a0, -8(fp)
      sd a1, -16(fp)
      sd a2, -24(fp)
      sd a3, -32(fp)
      sd a4, -40(fp)
      sd a5, -48(fp)
      sd a6, -56(fp)
      ld t0, -8(fp)
      ld t1, -16(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -64(fp)
      ld t0, -64(fp)
      ld t1, -24(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -72(fp)
      ld t0, -72(fp)
      ld t1, -32(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -80(fp)
      ld t0, -80(fp)
      ld t1, -40(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -88(fp)
      ld t0, -88(fp)
      ld t1, -48(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -96(fp)
      ld t0, -96(fp)
      ld t1, -56(fp)
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      mv t1, fp
      addi sp, sp, -816
      addi fp, sp, 800
      sd ra, 8(fp)
      sd t1, 0(fp)
      la a0, add
      li a1, 7
      call alloc_closure
      li a1, 4
      addi sp, sp, -32
      li t0, 3
      sd t0, 0(sp)
      li t0, 3
      sd t0, 8(sp)
      li t0, 3
      sd t0, 16(sp)
      li t0, 3
      sd t0, 24(sp)
      mv a2, sp
      call eml_applyN
      addi sp, sp, 32
      sd a0, -8(fp)
      ld a0, -8(fp)
      li a1, 2
      addi sp, sp, -16
      li t0, 3
      sd t0, 0(sp)
      li t0, 3
      sd t0, 8(sp)
      mv a2, sp
      call eml_applyN
      addi sp, sp, 16
      sd a0, -16(fp)
      ld a0, -16(fp)
      li a1, 2
      addi sp, sp, -16
      li t0, 3
      sd t0, 0(sp)
      li t0, 3
      sd t0, 8(sp)
      mv a2, sp
      call eml_applyN
      addi sp, sp, 16
      sd a0, -24(fp)
      ld a0, -24(fp)
      call print_int
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      li a0, 0
      ret
  |}]
;;
