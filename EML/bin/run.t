Test factorial compilation and execution
========================================
  $ ./main.exe -fromfile fact
  $ riscv64-linux-gnu-as -march=rv64gc a.s -o temp.o
  $ riscv64-linux-gnu-ld temp.o -o a.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./a.exe
  [120]

Test generated assembly structure
=================================

  $ cat a.s
  .text
  .globl _start
      # Factorial Compiler
      # Entry point
  _start:
      li a0, 5
      li t0, 1
      li t1, 1
      # Loop: while i <= n
  loop:
      slt t2, a0, t1
      bne t2, zero, done
      # Multiply result by i
      mul t0, t0, t1
      # Increment i
      addi t1, t1, 1
      jal zero, loop
  done:
      mv a0, t0
      li a7, 93
      ecall

Test with different input (optional)
====================================

  $ echo "let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1) in factorial 6" > ../test_fact.ml
  $ ./main.exe -fromfile ../test_fact.ml -o test.s
  $ riscv64-linux-gnu-as -march=rv64gc test.s -o test.o
  $ riscv64-linux-gnu-ld test.o -o test.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu -cpu rv64 ./test.exe
  [120]