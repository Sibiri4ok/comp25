	.text
	.globl llvm_call_indirect
	.type llvm_call_indirect, @function
llvm_call_indirect:
	pushq	%rbp
	movq	%rsp, %rbp
	movq	%rdi, %rax
	movq	%rsi, %r10
	movq	%rdx, %r11

	cmpq	$6, %r11
	jbe	.Lload_regs

	pushq	%r12
	movq	%rax, %r12
	pushq	%r13
	pushq	%r14

	leaq	-6(%r11), %rcx
	shlq	$3, %rcx
	movq	%rsp, %rax
	subq	%rcx, %rax
	andq	$15, %rax
	addq	%rax, %rcx
	movq	%rcx, %r13
	subq	%rcx, %rsp

	movq	$6, %r8
.Lstack_loop:
	cmpq	%r8, %r11
	jle	.Lrestore_fn
	movq	(%r10,%r8,8), %rax
	movq	%r8, %rcx
	subq	$6, %rcx
	movq	%rax, 0(%rsp,%rcx,8)
	incq	%r8
	jmp	.Lstack_loop

.Lrestore_fn:
	movq	%r12, %rax

.Lload_regs:
	movq	(%r10), %rdi
	movq	8(%r10), %rsi
	movq	16(%r10), %rdx
	movq	24(%r10), %rcx
	movq	32(%r10), %r8
	movq	40(%r10), %r9

	movq	%r11, %r14
	call	*%rax
	movq	%r14, %r11

	cmpq	$6, %r11
	jbe	.Lepilogue
	addq	%r13, %rsp
	popq	%r14
	popq	%r13
	popq	%r12

.Lepilogue:
	leave
	ret
	.size llvm_call_indirect, .-llvm_call_indirect
	.section .note.GNU-stack,"",%progbits
