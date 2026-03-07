Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ EML -o fact.s <<EOF
  > let rec fac x = if x = 0 then 1 else x * fac (x - 1)
  > 
  > let main = print_int (fac 4)

  $ make compile fact.s
  24

  $ EML -o fibo.s <<EOF
  > let rec fib x = if x <= 1 then x else fib (x - 1) + fib (x - 2)
  > 
  > let main = print_int (fib 6)

  $ make compile fibo.s
  8

====================== without gc ======================

  $ make compile many_tests/typed/001fac.ml
  24

  $ make compile many_tests/typed/003fib.ml
  33

  $ make compile many_tests/typed/004manyargs.ml
  1111111111110100

  $ make compile many_tests/typed/005fix.ml
  720

  $ make compile many_tests/typed/006partial2.ml
  1237



  $ make compile many_tests/typed/010fac_anf.ml

  $ make compile many_tests/typed/010faccps_ll.ml
  24

  $ make compile many_tests/typed/010fibcps_ll.ml
  8

