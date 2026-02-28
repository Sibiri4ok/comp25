[@@@ocaml.text "/*"]

(** Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open EML_lib
open Frontend
open Parser

let run str =
  match parse str with
  | Ok ast ->
    (match Middleend.Anf.anf_program ast with
     | Error e_anf -> Format.eprintf "ANF transformation error: %s\n%!" e_anf
     | Ok anf_ast ->
       Format.printf "%a\n%!" (Backend.Ricsv.Runner.gen_program) anf_ast)
  | Error _ -> Format.printf "Parsing error\n"
;;

let%expect_test "codegen default bin op" =
  run
    {|
  let foo =
    let a = 1 + 2 in
    let b = 3 - 4 in
    let c = 5 * 6 in
    let d = 7 <= 8 in
    let e = 9 >= 10 in
    let f = 11 = 12 in
    let g = 13 <> 14 in
    a
  ;;
  |};
  [%expect
    {|
    .section .text
      .globl foo
      .type foo, @function
    foo:
      addi sp, sp, -72
      sd ra, 64(sp)
      sd fp, 56(sp)
      addi fp, sp, 56
      li t0, 3
      li t1, 5
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -8(fp)
      li t0, 7
      li t1, 9
      sub a0, t0, t1
      addi a0, a0, 1
      sd a0, -16(fp)
      li t0, 11
      li t1, 13
      srli t0, t0, 1
      addi t1, t1, -1
      mul a0, t0, t1
      addi a0, a0, 1
      sd a0, -24(fp)
      li t0, 15
      li t1, 17
      slt a0, t1, t0
      xori a0, a0, 1
      sd a0, -32(fp)
      li t0, 19
      li t1, 21
      slt a0, t0, t1
      xori a0, a0, 1
      sd a0, -40(fp)
      li t0, 23
      li t1, 25
      xor a0, t0, t1
      seqz a0, a0
      sd a0, -48(fp)
      li t0, 27
      li t1, 29
      xor a0, t0, t1
      snez a0, a0
      sd a0, -56(fp)
      ld a0, -8(fp)
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret
  |}]
;;

let%expect_test "codegen ANF bin op" =
  run
    {|
  let foo = 1 + 2
  |};
  [%expect
    {|
    .section .text
      .globl foo
      .type foo, @function
    foo:
      addi sp, sp, -16
      sd ra, 8(sp)
      sd fp, 0(sp)
      addi fp, sp, 0
      li t0, 3
      li t1, 5
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret
  |}]
;;

let%expect_test "codegen default main function" =
  run
    {|
  let id x = x

  let main = 
    let temp1 = id 4 in
    temp1
  ;;
  |};
  [%expect
    {|
  .section .text
    .globl id
    .type id, @function
  id:
    addi sp, sp, -16
    sd ra, 8(sp)
    sd fp, 0(sp)
    addi fp, sp, 0
    addi sp, fp, 16
    ld ra, 8(fp)
    ld fp, 0(fp)
    ret

    .globl main
    .type main, @function
  main:
    addi sp, sp, -24
    sd ra, 16(sp)
    sd fp, 8(sp)
    addi fp, sp, 8
    li a0, 9
    call id
    sd a0, -8(fp)
    ld a0, -8(fp)
    addi sp, fp, 16
    ld ra, 8(fp)
    ld fp, 0(fp)
    li a0, 0
    ret
  |}]
;;

let%expect_test "codegen default factorial" =
  run
    {|
  let rec fac =
    fun n ->
    let temp1 = n = 0 in
    let temp5 =
      if temp1
      then 1
      else (
        let temp2 = n - 1 in
        let temp3 = fac temp2 in
        let temp4 = n * temp3 in
        temp4)
    in
    let temp6 = temp5 in
    temp6
  ;;
  |};
  [%expect
    {|
  .section .text
    .globl fac
    .type fac, @function
  fac:
    addi sp, sp, -64
    sd ra, 56(sp)
    sd fp, 48(sp)
    addi fp, sp, 48
    mv t0, a0
    li t1, 1
    mv a1, a0
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(fp)
    ld t0, -8(fp)
    beq t0, zero, else_0
    li a0, 3
    j end_0
  else_0:
    mv t0, a1
    li t1, 3
    sub a0, t0, t1
    addi a0, a0, 1
    sd a0, -16(fp)
    addi sp, sp, -8
    sd a1, -24(fp)
    ld a0, -16(fp)
    call fac
    sd a0, -32(fp)
    ld t0, -24(fp)
    ld t1, -32(fp)
    srli t0, t0, 1
    addi t1, t1, -1
    mul a0, t0, t1
    addi a0, a0, 1
    sd a0, -40(fp)
    ld a0, -40(fp)
  end_0:
    sd a0, -48(fp)
    ld a0, -48(fp)
    sd a0, -56(fp)
    ld a0, -56(fp)
    addi sp, fp, 16
    ld ra, 8(fp)
    ld fp, 0(fp)
    ret
  |}]
;;

let%expect_test "codegen ANF factorial" =
  run
    {|
  let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  |};
  [%expect
    {|
  .section .text
    .globl fac
    .type fac, @function
  fac:
    addi sp, sp, -40
    sd ra, 32(sp)
    sd fp, 24(sp)
    addi fp, sp, 24
    mv t0, a0
    li t1, 1
    mv a1, a0
    xor a0, t0, t1
    seqz a0, a0
    sd a0, -8(fp)
    ld t0, -8(fp)
    beq t0, zero, else_0
    li a0, 3
    j end_0
  else_0:
    mv t0, a1
    li t1, 3
    sub a0, t0, t1
    addi a0, a0, 1
    sd a0, -16(fp)
    addi sp, sp, -8
    sd a1, -24(fp)
    ld a0, -16(fp)
    call fac
    sd a0, -32(fp)
    ld t0, -24(fp)
    ld t1, -32(fp)
    srli t0, t0, 1
    addi t1, t1, -1
    mul a0, t0, t1
    addi a0, a0, 1
  end_0:
    addi sp, fp, 16
    ld ra, 8(fp)
    ld fp, 0(fp)
    ret
  |}]
;;

let%expect_test "codegen constant" =
  run
    {|
  let a = 1
  let main = print_int a
  |};
  [%expect
    {|
    .section .text
      .globl a
      .type a, @function
    a:
      addi sp, sp, -16
      sd ra, 8(sp)
      sd fp, 0(sp)
      addi fp, sp, 0
      li a0, 3
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -16
      sd ra, 8(sp)
      sd fp, 0(sp)
      addi fp, sp, 0
      addi sp, sp, -8
      call a
      sd a0, -8(fp)
      ld a0, -8(fp)
      call print_int
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
  let plus a b c d e f h i j k = a + b + c + d + e + f + h + i + j + k

  let main =
    let clos1 = plus 1 2 3 4 5 6 7 in
    let clos2 = clos1 8 in
    let clos3 = clos2 9 10 in
    print_int clos3
  ;;
  |};
  [%expect
    {|
    .section .text
      .globl plus
      .type plus, @function
    plus:
      addi sp, sp, -80
      sd ra, 72(sp)
      sd fp, 64(sp)
      addi fp, sp, 64
      mv t0, a0
      mv t1, a1
      sd a0, -8(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -16(fp)
      ld t0, -16(fp)
      mv t1, a2
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -24(fp)
      ld t0, -24(fp)
      mv t1, a3
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -32(fp)
      ld t0, -32(fp)
      mv t1, a4
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -40(fp)
      ld t0, -40(fp)
      mv t1, a5
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -48(fp)
      ld t0, -48(fp)
      mv t1, a6
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -56(fp)
      ld t0, -56(fp)
      mv t1, a7
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -64(fp)
      ld t0, -64(fp)
      ld t1, 16(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -72(fp)
      ld t0, -72(fp)
      ld t1, 24(fp)
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -40
      sd ra, 32(sp)
      sd fp, 24(sp)
      addi fp, sp, 24
      la a0, plus
      li a1, 10
      call alloc_closure
      addi sp, sp, -8
      sd a0, 0(sp)
      addi sp, sp, -56
      li t0, 3
      sd t0, 0(sp)
      li t0, 5
      sd t0, 8(sp)
      li t0, 7
      sd t0, 16(sp)
      li t0, 9
      sd t0, 24(sp)
      li t0, 11
      sd t0, 32(sp)
      li t0, 13
      sd t0, 40(sp)
      li t0, 15
      sd t0, 48(sp)
      ld a0, 56(sp)
      li a1, 7
      mv a2, sp
      call eml_applyN
      addi sp, sp, 64
      sd a0, -8(fp)
      ld a0, -8(fp)
      addi sp, sp, -8
      sd a0, 0(sp)
      addi sp, sp, -8
      li t0, 17
      sd t0, 0(sp)
      ld a0, 8(sp)
      li a1, 1
      mv a2, sp
      call eml_applyN
      addi sp, sp, 16
      sd a0, -16(fp)
      ld a0, -16(fp)
      addi sp, sp, -8
      sd a0, 0(sp)
      addi sp, sp, -16
      li t0, 19
      sd t0, 0(sp)
      li t0, 21
      sd t0, 8(sp)
      ld a0, 16(sp)
      li a1, 2
      mv a2, sp
      call eml_applyN
      addi sp, sp, 24
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

let%expect_test "codegen fn with 10 arg" =
  run
    {|
  let plus a b c d e f h i j k = a + b + c + d + e + f + h + i + j + k

  let main =
    let res = plus 1 2 3 4 5 6 7 8 9 10 in
    print_int res
  ;;
  |};
  [%expect
    {|
    .section .text
      .globl plus
      .type plus, @function
    plus:
      addi sp, sp, -80
      sd ra, 72(sp)
      sd fp, 64(sp)
      addi fp, sp, 64
      mv t0, a0
      mv t1, a1
      sd a0, -8(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -16(fp)
      ld t0, -16(fp)
      mv t1, a2
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -24(fp)
      ld t0, -24(fp)
      mv t1, a3
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -32(fp)
      ld t0, -32(fp)
      mv t1, a4
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -40(fp)
      ld t0, -40(fp)
      mv t1, a5
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -48(fp)
      ld t0, -48(fp)
      mv t1, a6
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -56(fp)
      ld t0, -56(fp)
      mv t1, a7
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -64(fp)
      ld t0, -64(fp)
      ld t1, 16(fp)
      add a0, t0, t1
      addi a0, a0, -1
      sd a0, -72(fp)
      ld t0, -72(fp)
      ld t1, 24(fp)
      add a0, t0, t1
      addi a0, a0, -1
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      ret

      .globl main
      .type main, @function
    main:
      addi sp, sp, -24
      sd ra, 16(sp)
      sd fp, 8(sp)
      addi fp, sp, 8
      li a0, 3
      li a1, 5
      li a2, 7
      li a3, 9
      li a4, 11
      li a5, 13
      li a6, 15
      li a7, 17
      addi sp, sp, -16
      li t0, 19
      sd t0, 0(sp)
      li t0, 21
      sd t0, 8(sp)
      call plus
      addi sp, sp, 16
      sd a0, -8(fp)
      ld a0, -8(fp)
      call print_int
      addi sp, fp, 16
      ld ra, 8(fp)
      ld fp, 0(fp)
      li a0, 0
      ret
  |}]
;;