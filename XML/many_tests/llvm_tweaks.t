  $ clang-18 --target=riscv64-linux-gnu --sysroot=/usr/riscv64-unknown-linux-gnu -c ./../bin/runtime.c -o runtime.o

====================== O0 ======================

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/010faccps_ll.ml -o temp.ll -O O0

  $ cat temp.ll
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
  
  define i64 @id(i64 %x) {
  entry:
    %x1 = alloca i64, align 8
    store i64 %x, ptr %x1, align 8
    %x2 = load i64, ptr %x1, align 8
    ret i64 %x2
  }
  
  define i64 @fresh_1(i64 %n, i64 %k, i64 %p) {
  entry:
    %t_2 = alloca i64, align 8
    %t_1 = alloca i64, align 8
    %p3 = alloca i64, align 8
    %k2 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 8
    store i64 %k, ptr %k2, align 8
    store i64 %p, ptr %p3, align 8
    %p4 = load i64, ptr %p3, align 8
    %n5 = load i64, ptr %n1, align 8
    %multmp1 = lshr i64 %p4, 1
    %multmp2 = sub i64 %n5, 1
    %multmp3 = mul i64 %multmp1, %multmp2
    %multmp4 = add i64 %multmp3, 1
    store i64 %multmp4, ptr %t_1, align 8
    %k_val = load i64, ptr %k2, align 8
    %t_16 = load i64, ptr %t_1, align 8
    %apptmp = call i64 @apply1(i64 %k_val, i64 %t_16)
    store i64 %apptmp, ptr %t_2, align 8
    %t_27 = load i64, ptr %t_2, align 8
    ret i64 %t_27
  }
  
  define i64 @fac_cps(i64 %n, i64 %k) {
  entry:
    %t_11 = alloca i64, align 8
    %t_10 = alloca i64, align 8
    %t_9 = alloca i64, align 8
    %t_8 = alloca i64, align 8
    %t_7 = alloca i64, align 8
    %t_6 = alloca i64, align 8
    %t_5 = alloca i64, align 8
    %t_4 = alloca i64, align 8
    %k2 = alloca i64, align 8
    %n1 = alloca i64, align 8
    store i64 %n, ptr %n1, align 8
    store i64 %k, ptr %k2, align 8
    %n3 = load i64, ptr %n1, align 8
    %eqtmp = icmp eq i64 %n3, 3
    %eqtmp_as_i64 = zext i1 %eqtmp to i64
    store i64 %eqtmp_as_i64, ptr %t_4, align 8
    %t_44 = load i64, ptr %t_4, align 8
    %cond = icmp ne i64 %t_44, 0
    br i1 %cond, label %then, label %else
  
  then:                                             ; preds = %entry
    %k_val = load i64, ptr %k2, align 8
    %apptmp = call i64 @apply1(i64 %k_val, i64 3)
    store i64 %apptmp, ptr %t_5, align 8
    %t_55 = load i64, ptr %t_5, align 8
    br label %ifcont
  
  else:                                             ; preds = %entry
    %n6 = load i64, ptr %n1, align 8
    %subtmp1 = sub i64 %n6, 3
    %subtmp2 = add i64 %subtmp1, 1
    store i64 %subtmp2, ptr %t_6, align 8
    %t_67 = load i64, ptr %t_6, align 8
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %apptmp8 = call i64 @apply1(i64 %closure_tmp, i64 %t_67)
    store i64 %apptmp8, ptr %t_7, align 8
    %n9 = load i64, ptr %n1, align 8
    %closure_tmp10 = call i64 @alloc_closure(i64 ptrtoint (ptr @fresh_1 to i64), i64 3)
    %apptmp11 = call i64 @apply1(i64 %closure_tmp10, i64 %n9)
    store i64 %apptmp11, ptr %t_8, align 8
    %t_8_val = load i64, ptr %t_8, align 8
    %k12 = load i64, ptr %k2, align 8
    %apptmp13 = call i64 @apply1(i64 %t_8_val, i64 %k12)
    store i64 %apptmp13, ptr %t_9, align 8
    %t_7_val = load i64, ptr %t_7, align 8
    %t_914 = load i64, ptr %t_9, align 8
    %apptmp15 = call i64 @apply1(i64 %t_7_val, i64 %t_914)
    store i64 %apptmp15, ptr %t_10, align 8
    %t_1016 = load i64, ptr %t_10, align 8
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %t_55, %then ], [ %t_1016, %else ]
    store i64 %iftmp, ptr %t_11, align 8
    %t_1117 = load i64, ptr %t_11, align 8
    ret i64 %t_1117
  }
  
  define i64 @main() {
  entry:
    %main = alloca i64, align 8
    %t_15 = alloca i64, align 8
    %t_14 = alloca i64, align 8
    %t_13 = alloca i64, align 8
    call void @rt_init(i64 5120)
    %closure_tmp = call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %apptmp = call i64 @apply1(i64 %closure_tmp, i64 9)
    store i64 %apptmp, ptr %t_13, align 8
    %t_13_val = load i64, ptr %t_13, align 8
    %closure_tmp1 = call i64 @alloc_closure(i64 ptrtoint (ptr @id to i64), i64 1)
    %apptmp2 = call i64 @apply1(i64 %t_13_val, i64 %closure_tmp1)
    store i64 %apptmp2, ptr %t_14, align 8
    %t_143 = load i64, ptr %t_14, align 8
    call void @print_int(i64 %t_143)
    store i64 0, ptr %t_15, align 8
    store i64 1, ptr %main, align 8
    call void @collect()
    ret i64 0
  }

  $ llc-18 temp.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  24

====================== O1 ======================

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/010faccps_ll.ml -o temp.ll -O O1

  $ cat temp.ll
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64) local_unnamed_addr
  
  declare i64 @alloc_closure(i64, i64) local_unnamed_addr
  
  declare i64 @apply1(i64, i64) local_unnamed_addr
  
  declare void @collect() local_unnamed_addr
  
  declare void @rt_init(i64) local_unnamed_addr
  
  ; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
  define i64 @id(i64 returned %x) #0 {
  entry:
    ret i64 %x
  }
  
  define i64 @fresh_1(i64 %n, i64 %k, i64 %p) {
  entry:
    %multmp1 = lshr i64 %p, 1
    %multmp2 = add i64 %n, -1
    %multmp3 = mul i64 %multmp1, %multmp2
    %multmp4 = add i64 %multmp3, 1
    %apptmp = tail call i64 @apply1(i64 %k, i64 %multmp4)
    ret i64 %apptmp
  }
  
  define i64 @fac_cps(i64 %n, i64 %k) {
  entry:
    %eqtmp = icmp eq i64 %n, 3
    br i1 %eqtmp, label %then, label %else
  
  then:                                             ; preds = %entry
    %apptmp = tail call i64 @apply1(i64 %k, i64 3)
    br label %ifcont
  
  else:                                             ; preds = %entry
    %subtmp2 = add i64 %n, -2
    %closure_tmp = tail call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %apptmp8 = tail call i64 @apply1(i64 %closure_tmp, i64 %subtmp2)
    %closure_tmp10 = tail call i64 @alloc_closure(i64 ptrtoint (ptr @fresh_1 to i64), i64 3)
    %apptmp11 = tail call i64 @apply1(i64 %closure_tmp10, i64 %n)
    %apptmp13 = tail call i64 @apply1(i64 %apptmp11, i64 %k)
    %apptmp15 = tail call i64 @apply1(i64 %apptmp8, i64 %apptmp13)
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %apptmp, %then ], [ %apptmp15, %else ]
    ret i64 %iftmp
  }
  
  define noundef i64 @main() local_unnamed_addr {
  entry:
    tail call void @rt_init(i64 5120)
    %closure_tmp = tail call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %apptmp = tail call i64 @apply1(i64 %closure_tmp, i64 9)
    %closure_tmp1 = tail call i64 @alloc_closure(i64 ptrtoint (ptr @id to i64), i64 1)
    %apptmp2 = tail call i64 @apply1(i64 %apptmp, i64 %closure_tmp1)
    tail call void @print_int(i64 %apptmp2)
    tail call void @collect()
    ret i64 0
  }
  
  attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }

  $ llc-18 temp.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  24

====================== O2 ======================

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/010faccps_ll.ml -o temp.ll -O O2

  $ cat temp.ll
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64) local_unnamed_addr
  
  declare i64 @alloc_closure(i64, i64) local_unnamed_addr
  
  declare i64 @apply1(i64, i64) local_unnamed_addr
  
  declare void @collect() local_unnamed_addr
  
  declare void @rt_init(i64) local_unnamed_addr
  
  ; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
  define i64 @id(i64 returned %x) #0 {
  entry:
    ret i64 %x
  }
  
  define i64 @fresh_1(i64 %n, i64 %k, i64 %p) {
  entry:
    %multmp1 = lshr i64 %p, 1
    %multmp2 = add i64 %n, -1
    %multmp3 = mul i64 %multmp1, %multmp2
    %multmp4 = add i64 %multmp3, 1
    %apptmp = tail call i64 @apply1(i64 %k, i64 %multmp4)
    ret i64 %apptmp
  }
  
  define i64 @fac_cps(i64 %n, i64 %k) {
  entry:
    %eqtmp = icmp eq i64 %n, 3
    br i1 %eqtmp, label %then, label %else
  
  then:                                             ; preds = %entry
    %apptmp = tail call i64 @apply1(i64 %k, i64 3)
    br label %ifcont
  
  else:                                             ; preds = %entry
    %subtmp2 = add i64 %n, -2
    %closure_tmp = tail call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %apptmp8 = tail call i64 @apply1(i64 %closure_tmp, i64 %subtmp2)
    %closure_tmp10 = tail call i64 @alloc_closure(i64 ptrtoint (ptr @fresh_1 to i64), i64 3)
    %apptmp11 = tail call i64 @apply1(i64 %closure_tmp10, i64 %n)
    %apptmp13 = tail call i64 @apply1(i64 %apptmp11, i64 %k)
    %apptmp15 = tail call i64 @apply1(i64 %apptmp8, i64 %apptmp13)
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %apptmp, %then ], [ %apptmp15, %else ]
    ret i64 %iftmp
  }
  
  define noundef i64 @main() local_unnamed_addr {
  entry:
    tail call void @rt_init(i64 5120)
    %closure_tmp = tail call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %apptmp = tail call i64 @apply1(i64 %closure_tmp, i64 9)
    %closure_tmp1 = tail call i64 @alloc_closure(i64 ptrtoint (ptr @id to i64), i64 1)
    %apptmp2 = tail call i64 @apply1(i64 %apptmp, i64 %closure_tmp1)
    tail call void @print_int(i64 %apptmp2)
    tail call void @collect()
    ret i64 0
  }
  
  attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }

  $ llc-18 temp.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  24

====================== O3 ======================

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/010faccps_ll.ml -o temp.ll -O O3

  $ cat temp.ll
  ; ModuleID = 'main'
  source_filename = "main"
  target triple = "riscv64-unknown-linux-gnu"
  
  declare void @print_int(i64) local_unnamed_addr
  
  declare i64 @alloc_closure(i64, i64) local_unnamed_addr
  
  declare i64 @apply1(i64, i64) local_unnamed_addr
  
  declare void @collect() local_unnamed_addr
  
  declare void @rt_init(i64) local_unnamed_addr
  
  ; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
  define i64 @id(i64 returned %x) #0 {
  entry:
    ret i64 %x
  }
  
  define i64 @fresh_1(i64 %n, i64 %k, i64 %p) {
  entry:
    %multmp1 = lshr i64 %p, 1
    %multmp2 = add i64 %n, -1
    %multmp3 = mul i64 %multmp1, %multmp2
    %multmp4 = add i64 %multmp3, 1
    %apptmp = tail call i64 @apply1(i64 %k, i64 %multmp4)
    ret i64 %apptmp
  }
  
  define i64 @fac_cps(i64 %n, i64 %k) {
  entry:
    %eqtmp = icmp eq i64 %n, 3
    br i1 %eqtmp, label %then, label %else
  
  then:                                             ; preds = %entry
    %apptmp = tail call i64 @apply1(i64 %k, i64 3)
    br label %ifcont
  
  else:                                             ; preds = %entry
    %subtmp2 = add i64 %n, -2
    %closure_tmp = tail call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %apptmp8 = tail call i64 @apply1(i64 %closure_tmp, i64 %subtmp2)
    %closure_tmp10 = tail call i64 @alloc_closure(i64 ptrtoint (ptr @fresh_1 to i64), i64 3)
    %apptmp11 = tail call i64 @apply1(i64 %closure_tmp10, i64 %n)
    %apptmp13 = tail call i64 @apply1(i64 %apptmp11, i64 %k)
    %apptmp15 = tail call i64 @apply1(i64 %apptmp8, i64 %apptmp13)
    br label %ifcont
  
  ifcont:                                           ; preds = %else, %then
    %iftmp = phi i64 [ %apptmp, %then ], [ %apptmp15, %else ]
    ret i64 %iftmp
  }
  
  define noundef i64 @main() local_unnamed_addr {
  entry:
    tail call void @rt_init(i64 5120)
    %closure_tmp = tail call i64 @alloc_closure(i64 ptrtoint (ptr @fac_cps to i64), i64 2)
    %apptmp = tail call i64 @apply1(i64 %closure_tmp, i64 9)
    %closure_tmp1 = tail call i64 @alloc_closure(i64 ptrtoint (ptr @id to i64), i64 1)
    %apptmp2 = tail call i64 @apply1(i64 %apptmp, i64 %closure_tmp1)
    tail call void @print_int(i64 %apptmp2)
    tail call void @collect()
    ret i64 0
  }
  
  attributes #0 = { mustprogress nofree norecurse nosync nounwind willreturn memory(none) }

  $ llc-18 temp.ll -o temp.s
  $ clang-18  --target=riscv64-linux-gnu -static temp.s runtime.o -o temp.exe
  $ qemu-riscv64 -L /usr/riscv64-linux-gnu/ -cpu rv64 ./temp.exe
  24

====================== RISC-V ======================

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/010faccps_ll.ml -o temp.ll -t "riscv64-unknown-linux-gnu"
  $ llc-18 temp.ll -o temp.s
  $ cat temp.s
  	.text
  	.attribute	4, 16
  	.attribute	5, "rv64i2p1"
  	.file	"main"
  	.globl	id                              # -- Begin function id
  	.p2align	2
  	.type	id,@function
  id:                                     # @id
  	.cfi_startproc
  # %bb.0:                                # %entry
  	addi	sp, sp, -16
  	.cfi_def_cfa_offset 16
  	sd	a0, 8(sp)
  	addi	sp, sp, 16
  	ret
  .Lfunc_end0:
  	.size	id, .Lfunc_end0-id
  	.cfi_endproc
                                          # -- End function
  	.globl	fresh_1                         # -- Begin function fresh_1
  	.p2align	2
  	.type	fresh_1,@function
  fresh_1:                                # @fresh_1
  	.cfi_startproc
  # %bb.0:                                # %entry
  	addi	sp, sp, -64
  	.cfi_def_cfa_offset 64
  	sd	ra, 56(sp)                      # 8-byte Folded Spill
  	sd	s0, 48(sp)                      # 8-byte Folded Spill
  	.cfi_offset ra, -8
  	.cfi_offset s0, -16
  	mv	s0, a1
  	sd	a0, 8(sp)
  	sd	a1, 16(sp)
  	sd	a2, 24(sp)
  	srli	a2, a2, 1
  	addi	a1, a0, -1
  	mv	a0, a2
  	call	__muldi3
  	addi	a1, a0, 1
  	sd	a1, 32(sp)
  	mv	a0, s0
  	call	apply1
  	sd	a0, 40(sp)
  	ld	ra, 56(sp)                      # 8-byte Folded Reload
  	ld	s0, 48(sp)                      # 8-byte Folded Reload
  	addi	sp, sp, 64
  	ret
  .Lfunc_end1:
  	.size	fresh_1, .Lfunc_end1-fresh_1
  	.cfi_endproc
                                          # -- End function
  	.globl	fac_cps                         # -- Begin function fac_cps
  	.p2align	2
  	.type	fac_cps,@function
  fac_cps:                                # @fac_cps
  	.cfi_startproc
  # %bb.0:                                # %entry
  	addi	sp, sp, -96
  	.cfi_def_cfa_offset 96
  	sd	ra, 88(sp)                      # 8-byte Folded Spill
  	sd	s0, 80(sp)                      # 8-byte Folded Spill
  	.cfi_offset ra, -8
  	.cfi_offset s0, -16
  	sd	a1, 8(sp)
  	addi	a1, a0, -3
  	seqz	a1, a1
  	sd	a1, 16(sp)
  	li	a1, 3
  	sd	a0, 0(sp)
  	bne	a0, a1, .LBB2_2
  # %bb.1:                                # %then
  	ld	a0, 8(sp)
  	li	a1, 3
  	call	apply1
  	sd	a0, 24(sp)
  	j	.LBB2_3
  .LBB2_2:                                # %else
  	ld	s0, 0(sp)
  	addi	s0, s0, -2
  	sd	s0, 32(sp)
  	lui	a0, %hi(fac_cps)
  	addi	a0, a0, %lo(fac_cps)
  	li	a1, 2
  	call	alloc_closure
  	mv	a1, s0
  	call	apply1
  	ld	s0, 0(sp)
  	sd	a0, 40(sp)
  	lui	a0, %hi(fresh_1)
  	addi	a0, a0, %lo(fresh_1)
  	li	a1, 3
  	call	alloc_closure
  	mv	a1, s0
  	call	apply1
  	ld	a1, 8(sp)
  	sd	a0, 48(sp)
  	call	apply1
  	ld	a1, 40(sp)
  	mv	a2, a0
  	sd	a0, 56(sp)
  	mv	a0, a1
  	mv	a1, a2
  	call	apply1
  	sd	a0, 64(sp)
  .LBB2_3:                                # %ifcont
  	sd	a0, 72(sp)
  	ld	ra, 88(sp)                      # 8-byte Folded Reload
  	ld	s0, 80(sp)                      # 8-byte Folded Reload
  	addi	sp, sp, 96
  	ret
  .Lfunc_end2:
  	.size	fac_cps, .Lfunc_end2-fac_cps
  	.cfi_endproc
                                          # -- End function
  	.globl	main                            # -- Begin function main
  	.p2align	2
  	.type	main,@function
  main:                                   # @main
  	.cfi_startproc
  # %bb.0:                                # %entry
  	addi	sp, sp, -64
  	.cfi_def_cfa_offset 64
  	sd	ra, 56(sp)                      # 8-byte Folded Spill
  	sd	s0, 48(sp)                      # 8-byte Folded Spill
  	sd	s1, 40(sp)                      # 8-byte Folded Spill
  	.cfi_offset ra, -8
  	.cfi_offset s0, -16
  	.cfi_offset s1, -24
  	li	a0, 5
  	slli	a0, a0, 10
  	call	rt_init
  	lui	a0, %hi(fac_cps)
  	addi	a0, a0, %lo(fac_cps)
  	li	a1, 2
  	call	alloc_closure
  	li	a1, 9
  	call	apply1
  	mv	s0, a0
  	sd	a0, 8(sp)
  	lui	a0, %hi(id)
  	addi	a0, a0, %lo(id)
  	li	a1, 1
  	li	s1, 1
  	call	alloc_closure
  	mv	a1, a0
  	mv	a0, s0
  	call	apply1
  	sd	a0, 16(sp)
  	call	print_int
  	sd	zero, 24(sp)
  	sd	s1, 32(sp)
  	call	collect
  	li	a0, 0
  	ld	ra, 56(sp)                      # 8-byte Folded Reload
  	ld	s0, 48(sp)                      # 8-byte Folded Reload
  	ld	s1, 40(sp)                      # 8-byte Folded Reload
  	addi	sp, sp, 64
  	ret
  .Lfunc_end3:
  	.size	main, .Lfunc_end3-main
  	.cfi_endproc
                                          # -- End function
  	.section	".note.GNU-stack","",@progbits

====================== x86-64 ======================

  $ dune exec -- ../bin/XML_llvm.exe -fromfile manytests/typed/010faccps_ll.ml -o temp.ll -t "x86_64-pc-linux-gnu" 
  $ llc-18 temp.ll -o temp.s
  $ cat temp.s
  	.text
  	.file	"main"
  	.globl	id                              # -- Begin function id
  	.p2align	4, 0x90
  	.type	id,@function
  id:                                     # @id
  	.cfi_startproc
  # %bb.0:                                # %entry
  	movq	%rdi, %rax
  	movq	%rdi, -8(%rsp)
  	retq
  .Lfunc_end0:
  	.size	id, .Lfunc_end0-id
  	.cfi_endproc
                                          # -- End function
  	.globl	fresh_1                         # -- Begin function fresh_1
  	.p2align	4, 0x90
  	.type	fresh_1,@function
  fresh_1:                                # @fresh_1
  	.cfi_startproc
  # %bb.0:                                # %entry
  	subq	$40, %rsp
  	.cfi_def_cfa_offset 48
  	movq	%rsi, %rax
  	movq	%rdi, (%rsp)
  	movq	%rsi, 8(%rsp)
  	movq	%rdx, 16(%rsp)
  	shrq	%rdx
  	leaq	-1(%rdi), %rsi
  	imulq	%rdx, %rsi
  	incq	%rsi
  	movq	%rsi, 24(%rsp)
  	movq	%rax, %rdi
  	callq	apply1@PLT
  	movq	%rax, 32(%rsp)
  	addq	$40, %rsp
  	.cfi_def_cfa_offset 8
  	retq
  .Lfunc_end1:
  	.size	fresh_1, .Lfunc_end1-fresh_1
  	.cfi_endproc
                                          # -- End function
  	.globl	fac_cps                         # -- Begin function fac_cps
  	.p2align	4, 0x90
  	.type	fac_cps,@function
  fac_cps:                                # @fac_cps
  	.cfi_startproc
  # %bb.0:                                # %entry
  	pushq	%rbx
  	.cfi_def_cfa_offset 16
  	subq	$80, %rsp
  	.cfi_def_cfa_offset 96
  	.cfi_offset %rbx, -16
  	movq	%rdi, (%rsp)
  	movq	%rsi, 8(%rsp)
  	xorl	%eax, %eax
  	cmpq	$3, %rdi
  	sete	%al
  	movq	%rax, 24(%rsp)
  	jne	.LBB2_2
  # %bb.1:                                # %then
  	movq	8(%rsp), %rdi
  	movl	$3, %esi
  	callq	apply1@PLT
  	movq	%rax, 32(%rsp)
  	jmp	.LBB2_3
  .LBB2_2:                                # %else
  	movq	(%rsp), %rbx
  	addq	$-2, %rbx
  	movq	%rbx, 40(%rsp)
  	movq	fac_cps@GOTPCREL(%rip), %rdi
  	movl	$2, %esi
  	callq	alloc_closure@PLT
  	movq	%rax, %rdi
  	movq	%rbx, %rsi
  	callq	apply1@PLT
  	movq	%rax, 16(%rsp)
  	movq	(%rsp), %rbx
  	movq	fresh_1@GOTPCREL(%rip), %rdi
  	movl	$3, %esi
  	callq	alloc_closure@PLT
  	movq	%rax, %rdi
  	movq	%rbx, %rsi
  	callq	apply1@PLT
  	movq	%rax, 48(%rsp)
  	movq	8(%rsp), %rsi
  	movq	%rax, %rdi
  	callq	apply1@PLT
  	movq	%rax, 56(%rsp)
  	movq	16(%rsp), %rdi
  	movq	%rax, %rsi
  	callq	apply1@PLT
  	movq	%rax, 64(%rsp)
  .LBB2_3:                                # %ifcont
  	movq	%rax, 72(%rsp)
  	addq	$80, %rsp
  	.cfi_def_cfa_offset 16
  	popq	%rbx
  	.cfi_def_cfa_offset 8
  	retq
  .Lfunc_end2:
  	.size	fac_cps, .Lfunc_end2-fac_cps
  	.cfi_endproc
                                          # -- End function
  	.globl	main                            # -- Begin function main
  	.p2align	4, 0x90
  	.type	main,@function
  main:                                   # @main
  	.cfi_startproc
  # %bb.0:                                # %entry
  	pushq	%rbx
  	.cfi_def_cfa_offset 16
  	subq	$32, %rsp
  	.cfi_def_cfa_offset 48
  	.cfi_offset %rbx, -16
  	movl	$5120, %edi                     # imm = 0x1400
  	callq	rt_init@PLT
  	movq	fac_cps@GOTPCREL(%rip), %rdi
  	movl	$2, %esi
  	callq	alloc_closure@PLT
  	movl	$9, %esi
  	movq	%rax, %rdi
  	callq	apply1@PLT
  	movq	%rax, %rbx
  	movq	%rax, (%rsp)
  	movq	id@GOTPCREL(%rip), %rdi
  	movl	$1, %esi
  	callq	alloc_closure@PLT
  	movq	%rbx, %rdi
  	movq	%rax, %rsi
  	callq	apply1@PLT
  	movq	%rax, 8(%rsp)
  	movq	%rax, %rdi
  	callq	print_int@PLT
  	movq	$0, 16(%rsp)
  	movq	$1, 24(%rsp)
  	callq	collect@PLT
  	xorl	%eax, %eax
  	addq	$32, %rsp
  	.cfi_def_cfa_offset 16
  	popq	%rbx
  	.cfi_def_cfa_offset 8
  	retq
  .Lfunc_end3:
  	.size	main, .Lfunc_end3-main
  	.cfi_endproc
                                          # -- End function
  	.section	".note.GNU-stack","",@progbits
