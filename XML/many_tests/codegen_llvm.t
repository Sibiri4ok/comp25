  $ clang-18 --target=riscv64-linux-gnu --sysroot=/usr/riscv64-unknown-linux-gnu -c ./../bin/runtime.c -o runtime.o

====================== Factorial ======================

  $ dune exec ./../bin/XML_llvm.exe -- -o factorial.ll <<EOF
  > let rec fac n = if n = 0 then 1 else n * fac (n - 1)
  > 
  > let main = print_int (fac 4)

  $ cat factorial.ll
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64)
  
  declare i64 @alloc_block(i64)
  
  declare i64 @alloc_closure(i64, i64)
  
  declare i64 @apply1(i64, i64)
  
  declare void @print_gc_status()
  
  declare void @collect()
  
  declare i64 @create_tuple(i64)
  
  declare i64 @create_tuple_init(i64, i64)
  
  declare i64 @field(i64, i64)
  
  declare void @rt_init(i64)
  
  define i64 @fac(i64 %n) {
  entry:
    %t_4 = alloca i64, align 8
    %t_3 = alloca i64, align 8
    %t_2 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %t_0 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 8
    %n2 = load i64, ptr %n1, align 8
    %eqtmp = icmp eq i64 %n2, 1
    %eqtmp_as_i64 = zext i1 %eqtmp to i64
    store i64 %eqtmp_as_i64, ptr %t_0, align 8
    %t_03 = load i64, ptr %t_0, align 8
    %cond = icmp ne i64 %t_03, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    br label %ifcont
  
  else:                                             ; preds = %entry
    %n4 = load i64, ptr %n1, align 8
    %subtmp1 = sub i64 %n4, 3
    %subtmp2 = add i64 %subtmp1, 1
    store i64 %subtmp2, ptr %t_1, align 8
    %t_15 = load i64, ptr %t_1, align 8
    %calltmp = call i64 @fac(i64 %t_15)
    store i64 %calltmp, ptr %t_2, align 8
    %n6 = load i64, ptr %n1, align 8
    %t_27 = load i64, ptr %t_2, align 8
    %multmp1 = lshr i64 %n6, 1
    %multmp2 = sub i64 %t_27, 1
    %multmp3 = mul i64 %multmp1, %multmp2
    %multmp4 = add i64 %multmp3, 1
    store i64 %multmp4, ptr %t_3, align 8
    %t_38 = load i64, ptr %t_3, align 8
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ 3, %then ], [ %t_38, %else ]
    store i64 %iftmp, ptr %t_4, align 8
    %t_49 = load i64, ptr %t_4, align 8
    ret i64 %t_49
  }
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %t_6 = alloca i64, align 8
    call void @rt_init(i64 5120)
    %calltmp = call i64 @fac(i64 9)
    store i64 %calltmp, ptr %t_6, align 8
    %t_61 = load i64, ptr %t_6, align 8
    call void @print_int(i64 %t_61)
    store i64 0, ptr %t_7, align 8
    %t_72 = load i64, ptr %t_7, align 8
    store i64 %t_72, ptr %main, align 8
    call void @collect()
    ret i64 0
  }

  $ llc-18 factorial.ll -o factorial.s
  $ clang-18  --target=riscv64-linux-gnu factorial.s runtime.o -o factorial.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./factorial.exe
  24

====================== Fibonacci ======================
  $ dune exec -- ../bin/XML_llvm.exe -o fibonacci.ll <<EOF
  > let rec fib n = if n <= 1 then n else fib (n - 1) + fib (n - 2)
  > 
  > let main = print_int (fib 6)


  $ llc-18 fibonacci.ll -o fibonacci.s
  $ clang-18  --target=riscv64-linux-gnu -static fibonacci.s runtime.o -o fibonacci.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./fibonacci.exe
  8

====================== Ififif ======================
  $ ../bin/XML_llvm.exe -o ififif.ll <<EOF
  > let large x = if 0<>x then print_int 0 else print_int 1
  > let main =
  >   let x = if (if (if 0 = 1
  >                   then 0 = 1 else (let t42 = print_int 42 in 1 = 1))
  >               then 0 else 1) = 1
  >           then 0 else 1 in
  >   large x

  $ llc-18 ififif.ll -o ififif.s
  $ clang-18  --target=riscv64-linux-gnu -static ififif.s runtime.o -o ififif.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./ififif.exe
  42
  0

====================== Simple Closure ======================
  $ dune exec -- ../bin/XML_llvm.exe -o closure.ll <<EOF
  > let simplesum x y = x + y
  > 
  > let partialapp_sum = simplesum 5
  > 
  > let main = print_int (partialapp_sum 5)

  $ llc-18 closure.ll -o closure.s
  $ clang-18  --target=riscv64-linux-gnu -static closure.s runtime.o -o closure.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./closure.exe
  10


====================== CPS Factorial ======================
  $ ../bin/XML_llvm.exe -fromfile manytests/typed/010faccps_ll.ml -o 010faccps_ll.ll

  $ llc-18 010faccps_ll.ll -o 010faccps_ll.s
  $ clang-18  --target=riscv64-linux-gnu -static 010faccps_ll.s runtime.o -o 010faccps_ll.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./010faccps_ll.exe
  24


====================== CPS Fibbo ======================
  $ ../bin/XML_llvm.exe -fromfile manytests/typed/010fibcps_ll.ml -o 010fibcps_ll.ll

  $ llc-18 010fibcps_ll.ll -o 010fibcps_ll.s
  $ clang-18  --target=riscv64-linux-gnu -static 010fibcps_ll.s runtime.o -o 010fibcps_ll.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./010fibcps_ll.exe
  8


====================== Other ======================

  $ ../bin/XML_llvm.exe -fromfile ./manytests/typed/004manyargs.ml -o 004manyargs.ll

  $ llc-18 004manyargs.ll -o 004manyargs.s
  $ clang-18  --target=riscv64-linux-gnu -static 004manyargs.s runtime.o -o 004manyargs.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./004manyargs.exe
  1111111111
  1
  10
  100



  $ ../bin/XML_llvm.exe -o tuple_return.ll <<EOF
  > let make_pair x y = (x, y)
  > 
  > let main =
  >   let p = make_pair 10 20 in
  >   let (a, b) = p in
  >   print_int (a + b)

  $ llc-18 tuple_return.ll -o tuple_return.s
  $ clang-18  --target=riscv64-linux-gnu -static tuple_return.s runtime.o -o tuple_return.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./tuple_return.exe
  30


 
  $ ../bin/XML_llvm.exe -o tuple_swap.ll <<EOF
  > let swap p =
  >   let (a, b) = p in
  >   (b, a)
  > 
  > let main =
  >   let p1 = (1, 2) in
  >   let p2 = swap p1 in
  >   let (x, y) = p2 in
  >   print_int x

  $ llc-18 tuple_swap.ll -o tuple_swap.s
  $ clang-18  --target=riscv64-linux-gnu -static tuple_swap.s runtime.o -o tuple_swap.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./tuple_swap.exe
  2


  $ ../bin/XML_llvm.exe -o tuple_order.ll <<EOF
  > let f n =
  >   n
  > 
  > let main =
  >   let t = (f 10, f 20) in
  >   let (a, b) = t in
  >   print_int (a + b)

  $ llc-18 tuple_order.ll -o tuple_order.s
  $ clang-18  --target=riscv64-linux-gnu -static tuple_order.s runtime.o -o tuple_order.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./tuple_order.exe
  30


  $ dune exec -- ../bin/XML_llvm.exe -o tuple_linked_list.ll -notypes <<EOF
  > let rec sum_list lst =
  >   if lst = 0 then 0 else
  >   let (head, tail) = lst in
  >   head + sum_list tail
  > 
  > let main =
  >   let lst = (10, (20, (30, 0))) in
  >   print_int (sum_list lst)


  $ llc-18 tuple_linked_list.ll -o tuple_linked_list.s
  $ clang-18  --target=riscv64-linux-gnu -static tuple_linked_list.s runtime.o -o tuple_linked_list.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./tuple_linked_list.exe
  60



  $ ../bin/XML_llvm.exe -o tuple_large.ll <<EOF
  > let main =
  >   let t = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) in
  >   let (a, b, c, d, e, f, g, h, i, j) = t in
  >   print_int j

  $ llc-18 tuple_large.ll -o tuple_large.s
  $ clang-18  --target=riscv64-linux-gnu -static tuple_large.s runtime.o -o tuple_large.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./tuple_large.exe
  10


  $ ../bin/XML_llvm.exe -o tuple_basic.ll <<EOF
  > let main =
  >   let t = (10, 20) in
  >   let (a, b) = t in
  >   print_int (a + b)

  $ llc-18 tuple_basic.ll -o tuple_basic.s
  $ clang-18  --target=riscv64-linux-gnu -static tuple_basic.s runtime.o -o tuple_basic.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./tuple_basic.exe
  30


  $ ../bin/XML_llvm.exe -o tuple_nested.ll <<EOF
  > let main =
  >   let complex = (100, (20, 3)) in
  >   let (a, (b, c)) = complex in
  >   print_int (a + b + c)


  $ llc-18 tuple_nested.ll -o tuple_nested.s
  $ clang-18  --target=riscv64-linux-gnu -static tuple_nested.s runtime.o -o tuple_nested.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./tuple_nested.exe
  123


  $ ../bin/XML_llvm.exe -o tuple_arg.ll <<EOF
  > let sum_pair p =
  >   let (x, y) = p in
  >   x + y
  > 
  > let main =
  >   let p = (40, 2) in
  >   print_int (sum_pair p)


  $ llc-18 tuple_arg.ll -o tuple_arg.s
  $ clang-18  --target=riscv64-linux-gnu  -static tuple_arg.s runtime.o -o tuple_arg.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./tuple_arg.exe
  42

  $ dune exec -- ../bin/XML_llvm.exe -o tuple_gc_stress.ll -notypes <<EOF
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

  $ llc-18 tuple_gc_stress.ll -o tuple_gc_stress.s
  $ clang-18  --target=riscv64-linux-gnu  -static tuple_gc_stress.s runtime.o -o tuple_gc_stress.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./tuple_gc_stress.exe
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

  $ ../bin/XML_llvm.exe -o user_main.ll -notypes <<EOF
  > let main x = x
  > 
  > let a = print_int (main 5)

  $ llc-18 user_main.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu  -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  5


  $ ../bin/XML_llvm.exe -o use_reserved_name.ll -notypes <<EOF
  > let some_fun x y = x + y
  > let closure_tmp x y = x + y
  > 
  > let a = print_int (some_fun 5 10)
  > let b = print_int (closure_tmp 5 10)
  > let closure_tmp = 5
  > let c = print_int closure_tmp

  $ llc-18 use_reserved_name.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu  -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  15
  15
  5
