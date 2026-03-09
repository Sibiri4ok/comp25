# x86-64 SysV: llvm_call_indirect(void *fn, void **args, int64_t n)
# Called from C runtime when invoking a closure. Loads args[0..n-1] into
# registers (rdi,rsi,rdx,rcx,r8,r9) and stack, then calls fn.

	.globl	llvm_call_indirect
	.type	llvm_call_indirect, @function
llvm_call_indirect:
	# rdi = fn, rsi = args, rdx = n
	movq	%rdi, %rax
	movq	%rsi, %r10
	movq	%rdx, %r11

	# First 6 args into registers (SysV: rdi, rsi, rdx, rcx, r8, r9)
	movq	0(%r10), %rdi
	movq	8(%r10), %rsi
	movq	16(%r10), %rdx
	movq	24(%r10), %rcx
	movq	32(%r10), %r8
	movq	40(%r10), %r9

	# If n > 6, push args[6..n-1] in reverse order (right-to-left)
	cmpq	$6, %r11
	jbe	.Ldo_call
	movq	%r11, %r12
	subq	$1, %r12		# r12 = n - 1 (last index)
.Lpush_loop:
	cmpq	$6, %r12
	jb	.Ldo_call
	pushq	(%r10,%r12,8)
	subq	$1, %r12
	jmp	.Lpush_loop
.Ldo_call:
	call	*%rax
	ret

	.size	llvm_call_indirect, .-llvm_call_indirect
