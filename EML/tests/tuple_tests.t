Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev
SPDX-License-Identifier: LGPL-3.0-or-later

  $ make compile_riscv GC=1 gc_tests/tuple_tests/01adder.ml
  === GC Status ===
  Current  allocated: 13
  Total    allocated: 13
  Free         space: 787
  Heap          size: 800
  Current bank index: 0
  GC     collections: 0
  GC     allocations: 3
  =================
  42
  === GC Status ===
  Current  allocated: 13
  Total    allocated: 28
  Free         space: 787
  Heap          size: 800
  Current bank index: 1
  GC     collections: 1
  GC     allocations: 4
  =================

  $ make compile_riscv GC=1 gc_tests/tuple_tests/02nested.ml
  === GC Status ===
  Current  allocated: 23
  Total    allocated: 23
  Free         space: 777
  Heap          size: 800
  Current bank index: 0
  GC     collections: 0
  GC     allocations: 4
  =================
  90
  === GC Status ===
  Current  allocated: 23
  Total    allocated: 48
  Free         space: 777
  Heap          size: 800
  Current bank index: 1
  GC     collections: 1
  GC     allocations: 5
  =================

  $ make compile_riscv GC=1 gc_tests/tuple_tests/03args.ml
  === GC Status ===
  Current  allocated: 28
  Total    allocated: 28
  Free         space: 772
  Heap          size: 800
  Current bank index: 0
  GC     collections: 0
  GC     allocations: 4
  =================
  1053
  === GC Status ===
  Current  allocated: 28
  Total    allocated: 58
  Free         space: 772
  Heap          size: 800
  Current bank index: 1
  GC     collections: 1
  GC     allocations: 5
  =================
