Copyright 2025-2026, Victoria Ostrovskaya & Danil Usoltsev
SPDX-License-Identifier: LGPL-3.0-or-later


  $ make compile_riscv GC=1 gc_tests/closure/01_add5_staged_partial_gc.ml
  === GC Status ===
  Current  allocated: 18
  Total    allocated: 18
  Free         space: 782
  Heap          size: 800
  Current bank index: 0
  GC     collections: 0
  GC     allocations: 2
  =================
  === GC Status ===
  Current  allocated: 27
  Total    allocated: 27
  Free         space: 773
  Heap          size: 800
  Current bank index: 0
  GC     collections: 0
  GC     allocations: 3
  =================
  === GC Status ===
  Current  allocated: 18
  Total    allocated: 45
  Free         space: 782
  Heap          size: 800
  Current bank index: 1
  GC     collections: 1
  GC     allocations: 3
  =================
  === GC Status ===
  Current  allocated: 27
  Total    allocated: 81
  Free         space: 773
  Heap          size: 800
  Current bank index: 0
  GC     collections: 2
  GC     allocations: 4
  =================
  15

  $ make compile_riscv GC=1 gc_tests/closure/02_affine_live_dead_gc.ml
  === GC Status ===
  Current  allocated: 28
  Total    allocated: 28
  Free         space: 772
  Heap          size: 800
  Current bank index: 0
  GC     collections: 0
  GC     allocations: 4
  =================
  === GC Status ===
  Current  allocated: 14
  Total    allocated: 42
  Free         space: 786
  Heap          size: 800
  Current bank index: 1
  GC     collections: 1
  GC     allocations: 4
  =================
  === GC Status ===
  Current  allocated: 28
  Total    allocated: 56
  Free         space: 772
  Heap          size: 800
  Current bank index: 1
  GC     collections: 1
  GC     allocations: 6
  =================
  === GC Status ===
  Current  allocated: 21
  Total    allocated: 77
  Free         space: 779
  Heap          size: 800
  Current bank index: 0
  GC     collections: 2
  GC     allocations: 6
  =================
  17

  $ make compile_riscv GC=1 gc_tests/closure/03_add10_staged_partial_gc.ml
  === GC Status ===
  Current  allocated: 28
  Total    allocated: 28
  Free         space: 772
  Heap          size: 800
  Current bank index: 0
  GC     collections: 0
  GC     allocations: 2
  =================
  === GC Status ===
  Current  allocated: 42
  Total    allocated: 42
  Free         space: 758
  Heap          size: 800
  Current bank index: 0
  GC     collections: 0
  GC     allocations: 3
  =================
  === GC Status ===
  Current  allocated: 28
  Total    allocated: 70
  Free         space: 772
  Heap          size: 800
  Current bank index: 1
  GC     collections: 1
  GC     allocations: 3
  =================
  === GC Status ===
  Current  allocated: 42
  Total    allocated: 126
  Free         space: 758
  Heap          size: 800
  Current bank index: 0
  GC     collections: 2
  GC     allocations: 4
  =================
  55
