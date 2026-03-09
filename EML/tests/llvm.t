Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ make compile_llvm many_tests/typed/001fac.ml
  24

  $ make compile_llvm many_tests/typed/003fib.ml
  3
  3

  $ make compile_llvm many_tests/typed/004manyargs.ml
  1111111111
  1
  10
  100

  $ make compile_llvm many_tests/typed/005fix.ml
  720

  $ make compile_llvm many_tests/typed/006partial2.ml
  1
  2
  3
  7

  $ make compile_llvm many_tests/typed/010fac_anf.ml

  $ make compile_llvm many_tests/typed/010faccps_ll.ml
  24

  $ make compile_llvm many_tests/typed/010fibcps_ll.ml
  8
