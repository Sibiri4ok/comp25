Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev
SPDX-License-Identifier: LGPL-3.0-or-later


  $ make infer many_tests/typed/001fac.ml
  val fac: int -> int
  val main: int

  $ make infer many_tests/typed/002fac.ml
  val fac_cps: int -> (int -> int) -> int
  val main: int

  $ make infer many_tests/typed/003fib.ml
  val fib: int -> int
  val fib_acc: int -> int -> int -> int
  val main: int

  $ make infer many_tests/typed/004manyargs.ml
  val main: int
  val test10: int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int
  val test3: int -> int -> int -> int
  val wrap: t0 -> t0

  $ make infer many_tests/typed/005fix.ml
  val fac: (int -> int) -> int -> int
  val fix: ((int -> int) -> int -> int) -> int -> int
  val main: int

  $ make infer many_tests/typed/006partial.ml
  val foo: int -> int
  val main: int

  $ make infer many_tests/typed/006partial2.ml
  val foo: int -> int -> int -> int
  val main: int

  $ make infer many_tests/typed/006partial3.ml
  val foo: int -> int -> int -> unit
  val main: int

  $ make infer many_tests/typed/007order.ml
  val _start: unit -> unit -> int -> unit -> int -> int -> unit -> int -> int -> int
  val main: unit

  $ make infer many_tests/typed/008ascription.ml
  val addi: (t2 -> bool -> int) -> (t2 -> bool) -> t2 -> int
  val main: int

  $ make infer many_tests/typed/009let_poly.ml
  val temp: (int * bool)

  $ make infer many_tests/typed/010fac_anf.ml
  val fac: int -> int
  val main: int

  $ make infer many_tests/typed/010faccps_ll.ml
  val fac_cps: int -> (int -> int) -> int
  val fresh_1: int -> (int -> t4) -> int -> t4
  val id: t0 -> t0
  val main: int

  $ make infer many_tests/typed/010fibcps_ll.ml
  val fib: int -> (int -> int) -> int
  val fresh_1: int -> (int -> t10) -> (int -> (int -> t10) -> t13) -> int -> t13
  val fresh_2: int -> (int -> t4) -> int -> t4
  val id: t0 -> t0
  val main: int

  $ make infer many_tests/typed/011mapcps.ml

  $ make infer many_tests/typed/012faccps.ml
  val fac: int -> (int -> int) -> int
  val main: unit

  $ make infer many_tests/typed/012fibcps.ml
  val fib: int -> (int -> int) -> int
  val main: unit

  $ make infer many_tests/typed/013foldfoldr.ml
  val fold_right: (int -> (int -> int) -> int -> int) -> (int -> int) -> int list -> int -> int
  val foldl: (int -> int -> int) -> int -> int list -> int
  val id: t0 -> t0
  val main: unit

  $ make infer many_tests/typed/015tuples.ml
  val feven: (t29 * int -> t33) -> int -> int
  val fix: ((((int -> int * int -> int) -> int -> int * (int -> int * int -> int) -> int -> int) -> (int -> int * int -> int)) -> ((int -> int * int -> int) -> int -> int * (int -> int * int -> int) -> int -> int) -> (int -> int * int -> int)) -> ((int -> int * int -> int) -> int -> int * (int -> int * int -> int) -> int -> int) -> (int -> int * int -> int)
  val fixpoly: ((int -> int * int -> int) -> int -> int * (int -> int * int -> int) -> int -> int) -> (int -> int * int -> int)
  val fodd: (int -> t40 * t37) -> int -> int
  val main: int
  val map: (t9 -> t11) -> (t9 * t9) -> (t10 * t11)
  val meven: int -> int
  val modd: int -> int
  val tie: (int -> int * int -> int)
