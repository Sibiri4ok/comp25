  $ clang-18 --target=riscv64-linux-gnu --sysroot=/usr/riscv64-unknown-linux-gnu -c ./../bin/runtime.c -o runtime.o

  $ dune exec ./../bin/XML_llvm.exe -- -o gc_smoke.ll <<EOF
  > let main =
  >   let _ = print_gc_status in
  >   let _ = collect in
  >   print_gc_status

  $ llc-18 gc_smoke.ll -o gc_smoke.s
  $ clang-18 --target=riscv64-linux-gnu --sysroot=/usr/riscv64-unknown-linux-gnu -c ./../bin/runtime.c -o runtime.o
  $ clang-18 --target=riscv64-linux-gnu -static gc_smoke.s runtime.o -o gc_smoke.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./gc_smoke.exe
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 0
  GC    collections: 0
  GC    allocations: 0
  =================
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 1
  Total   allocated: 0
  GC    collections: 1
  GC    allocations: 0
  =================



  $ dune exec ./../bin/XML_llvm.exe -- -o lots_of_garbage.ll <<EOF
  > let rec make_garbage n =
  >   if n = 0 then 0 else
  >     let _ = alloc_block 1 in
  >     make_garbage (n - 1)
  > let main =
  >   let _ = print_gc_status in
  >   let f = (fun x -> x) in
  >   let _ = make_garbage 2000 in
  >   let _ = print_gc_status in
  >   let _ = collect in
  >   let _ = print_gc_status in
  >   print_int (f 8)

  $ llc-18 lots_of_garbage.ll -o lots_of_garbage.s
  $ clang-18  --target=riscv64-linux-gnu -static lots_of_garbage.s runtime.o -o lots_of_garbage.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./lots_of_garbage.exe
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 0
  GC    collections: 0
  GC    allocations: 0
  =================
  === GC Status ===
  Current allocated: 80000
  Free        space: 444288
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 80000
  GC    collections: 0
  GC    allocations: 2000
  =================
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 1
  Total   allocated: 80000
  GC    collections: 1
  GC    allocations: 2000
  =================
  8



  $ dune exec ./../bin/XML_llvm.exe -- -o keep_block_across_gc.ll <<EOF
  > let rec spam n =
  >   if n = 0 then 0 else
  >     let _ = (fun z -> z) in
  >     spam (n - 1)
  > 
  > let make n =
  >   let b = alloc_block n in
  >   (fun x -> let _u = b in x)
  > 
  > let main =
  >   let f = make 10 in
  >   let _ = spam 4000 in
  >   let _ = collect in
  >   print_int (f 7)

  $ llc-18 keep_block_across_gc.ll -o keep_block_across_gc.s
  $ clang-18  --target=riscv64-linux-gnu -static keep_block_across_gc.s runtime.o -o keep_block_across_gc.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./keep_block_across_gc.exe
  7



  $ dune exec ./../bin/XML_llvm.exe -- -o gc_oom_block.ll <<EOF
  > let main =
  >   let _ = alloc_block 10000000 in
  >   print_int 0

  $ llc-18 gc_oom_block.ll -o gc_oom_block.s
  $ clang-18  --target=riscv64-linux-gnu -static gc_oom_block.s runtime.o -o gc_oom_block.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./gc_oom_block.exe
  GC: out of memory: asked for 160000024 bytes)
  Aborted (core dumped)
  [134]




  $ dune exec -- ../bin/XML_llvm.exe -o temp.ll -notypes <<EOF
  > let rec make_list n acc =
  >   if n = 0 then acc else
  >   make_list (n - 1) (n, acc)
  > 
  > let main =
  >   let _ = print_gc_status in
  >   let result = make_list 10000 0 in
  >   let (head, tail) = result in
  >   let _ = print_gc_status in
  >   print_int head

  $ llc-18 temp.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  === GC Status ===
  Current allocated: 0
  Free        space: 524288
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 0
  GC    collections: 0
  GC    allocations: 0
  =================
  === GC Status ===
  Current allocated: 231552
  Free        space: 292736
  Heap         size: 524288
  Current      bank: 0
  Total   allocated: 1280096
  GC    collections: 2
  GC    allocations: 30002
  =================
  1
