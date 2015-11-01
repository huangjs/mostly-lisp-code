# GNU C (GCC) version 4.3.0 20080201 (experimental) (i386-apple-darwin9.1.0)
#	compiled by GNU C version 4.3.0 20080201 (experimental), GMP version 4.2.2, MPFR version 2.3.1.
# GGC heuristics: --param ggc-min-expand=30 --param ggc-min-heapsize=4096
# options passed:  -imultilib x86_64 -D__DYNAMIC__ -DMAX_INDEX=512
# insert-and-sort.c -fPIC -mmacosx-version-min=10.5.3 -m64 -mtune=generic
# -O3 -std=gnu99 -fverbose-asm
# options enabled:  -fPIC -falign-loops -fargument-alias
# -fasynchronous-unwind-tables -fauto-inc-dec -fbranch-count-reg
# -fcaller-saves -fcommon -fcprop-registers -fcrossjumping
# -fcse-follow-jumps -fdefer-pop -fdelete-null-pointer-checks
# -fearly-inlining -feliminate-unused-debug-types -fexpensive-optimizations
# -fforward-propagate -ffunction-cse -fgcse -fgcse-after-reload -fgcse-lm
# -fguess-branch-probability -fident -fif-conversion -fif-conversion2
# -finline-functions -finline-functions-called-once
# -finline-small-functions -fipa-pure-const -fipa-reference -fivopts
# -fkeep-static-consts -fleading-underscore -fmerge-constants
# -fmerge-debug-strings -fmove-loop-invariants -foptimize-register-move
# -foptimize-sibling-calls -fpeephole -fpeephole2 -fpredictive-commoning
# -freg-struct-return -fregmove -freorder-blocks -freorder-functions
# -frerun-cse-after-loop -fsched-interblock -fsched-spec
# -fsched-stalled-insns-dep -fschedule-insns2 -fsigned-zeros
# -fsplit-ivs-in-unroller -fsplit-wide-types -fstrict-aliasing
# -fstrict-overflow -fthread-jumps -ftoplevel-reorder -ftrapping-math
# -ftree-ccp -ftree-ch -ftree-copy-prop -ftree-copyrename -ftree-cselim
# -ftree-dce -ftree-dominator-opts -ftree-dse -ftree-fre -ftree-loop-im
# -ftree-loop-ivcanon -ftree-loop-optimize -ftree-parallelize-loops=
# -ftree-pre -ftree-reassoc -ftree-salias -ftree-scev-cprop -ftree-sink
# -ftree-sra -ftree-store-ccp -ftree-ter -ftree-vect-loop-version
# -ftree-vectorize -ftree-vrp -funit-at-a-time -funswitch-loops
# -funwind-tables -fvect-cost-model -fverbose-asm -fzero-initialized-in-bss
# -m128bit-long-double -m64 -m80387 -maccumulate-outgoing-args
# -malign-stringops -mfancy-math-387 -mfp-ret-in-387 -mfused-madd -mieee-fp
# -mmmx -mno-sse4 -mpush-args -mred-zone -msse -msse2 -msse3

# Compiler executable checksum: 57cc6741ddbbe9993f85a2a6d525a3ef

	.text
	.align 4,0x90
.globl _popcount
_popcount:
LFB8:
	movq	%rdi, %rax	# x, tmp72
	movabsq	$6148914691236517205, %rdx	#, tmp73
	pushq	%rbp	#
LCFI0:
	shrq	%rax	# tmp72
	andq	%rdx, %rax	# tmp73, tmp72
	movabsq	$3689348814741910323, %rdx	#, tmp75
	movq	%rsp, %rbp	#,
LCFI1:
	subq	%rax, %rdi	# tmp72, x.28
	leave
	movq	%rdi, %rax	# x.28, tmp74
	andq	%rdx, %rdi	# tmp75, x.28
	shrq	$2, %rax	#, tmp74
	andq	%rdx, %rax	# tmp75, tmp74
	addq	%rdi, %rax	# x.28, x.29
	movq	%rax, %rdx	# x.29, tmp77
	shrq	$4, %rdx	#, tmp77
	addq	%rax, %rdx	# x.29, x.30
	movabsq	$1085102592571150095, %rax	#, tmp78
	andq	%rax, %rdx	# tmp78, x.30
	movq	%rdx, %rax	# x.30, D.2927
	shrq	$8, %rax	#, D.2927
	addq	%rdx, %rax	# x.30, x.31
	movq	%rax, %rdx	# x.31, D.2928
	shrq	$16, %rdx	#, D.2928
	addq	%rax, %rdx	# x.31, x.32
	movq	%rdx, %rax	# x.32, D.2929
	shrq	$32, %rax	#, D.2929
	addq	%rdx, %rax	# x.32, tmp80
	andl	$127, %eax	#, tmp79
	ret
LFE8:
	.align 4,0x90
.globl _insert_payload
_insert_payload:
LFB11:
	movq	96(%rdi), %rdx	# <variable>.log_count, D.2961
	movl	%esi, %r8d	# payload, payload$key
	movq	%rsi, %rax	# payload, tmp64
	pushq	%rbp	#
LCFI2:
	sarq	$32, %rax	#, tmp64
	andl	$63, %r8d	#, payload$key
	leaq	12(%rdx), %rcx	#, tmp63
	addq	$1, %rdx	#, tmp66
	movq	%rsp, %rbp	#,
LCFI3:
	movq	%rdx, 96(%rdi)	# tmp66, <variable>.log_count
	movl	%esi, %edx	# payload$key, index
	shrl	$6, %edx	#, index
	movl	%eax, 12(%rdi,%rcx,8)	# tmp64, <variable>.value
	movl	%esi, 8(%rdi,%rcx,8)	# payload, <variable>.key
	movslq	%edx,%rdx	# index, index
	movl	$1, %eax	#, tmp70
	movl	%r8d, %ecx	#,
	salq	%cl, %rax	#, tmp70
	orq	%rax, (%rdi,%rdx,8)	# tmp70, <variable>.masks
	leave
	ret
LFE11:
	.align 4,0x90
.globl _test_dummy
_test_dummy:
LFB13:
	pushq	%rbp	#
LCFI4:
	testq	%rsi, %rsi	# repeat
	movq	%rsp, %rbp	#,
LCFI5:
	je	L8	#,
	movq	96(%rdi), %r8	# <variable>.log_count, <variable>.log_count
	xorl	%ecx, %ecx	# i
	.align 4,0x90
L7:
	leaq	12(%r8), %rdx	#, tmp66
	movl	%ecx, %eax	#, tmp68
	addq	$1, %r8	#, <variable>.log_count
	andl	$511, %eax	#, tmp68
	movq	%r8, 96(%rdi)	# <variable>.log_count, <variable>.log_count
	movl	%ecx, 12(%rdi,%rdx,8)	# i, <variable>.value
	addq	$1, %rcx	#, i
	movl	%eax, 8(%rdi,%rdx,8)	# tmp68, <variable>.key
	cmpq	%rcx, %rsi	# i, repeat
	ja	L7	#,
L8:
	leave
	ret
LFE13:
	.align 4,0x90
.globl _test_insertion
_test_insertion:
LFB14:
	pushq	%rbp	#
LCFI6:
	testq	%rsi, %rsi	# repeat
	movq	%rsp, %rbp	#,
LCFI7:
	je	L20	#,
	xorl	%r8d, %r8d	# i
	.align 4,0x90
L19:
	movq	96(%rdi), %rdx	# <variable>.log_count, D.3021
	movl	%r8d, %ecx	#, D.3012
	movl	%r8d, %r9d	# i, D.3013
	andl	$511, %ecx	#, D.3012
	testq	%rdx, %rdx	# D.3021
	je	L13	#,
	xorl	%eax, %eax	# j
	cmpl	104(%rdi), %ecx	# <variable>.key, D.3012
	jne	L21	#,
	jmp	L15	#
	.align 4,0x90
L18:
	cmpl	104(%rdi,%rax,8), %ecx	# <variable>.key, D.3012
	je	L15	#,
L21:
	addq	$1, %rax	#, j
	cmpq	%rdx, %rax	# D.3021, j
	jb	L18	#,
L13:
	leaq	12(%rdx), %rax	#, tmp91
	movl	%r9d, 12(%rdi,%rax,8)	# D.3013, <variable>.value
	movl	%ecx, 8(%rdi,%rax,8)	# D.3012, <variable>.key
	leaq	1(%rdx), %rax	#, tmp93
	movq	%rax, 96(%rdi)	# tmp93, <variable>.log_count
L17:
	addq	$1, %r8	#, i
	cmpq	%r8, %rsi	# i, repeat
	ja	L19	#,
L20:
	leave
	ret
	.align 4,0x90
L15:
	addl	%r9d, 108(%rdi,%rax,8)	# D.3013, <variable>.value
	jmp	L17	#
LFE14:
	.align 4,0x90
.globl _init_range_count
_init_range_count:
LFB9:
	pushq	%rbp	#
LCFI8:
	movq	%rdi, %rdx	# range, range
	xorl	%eax, %eax	# tmp61
	movl	$13, %ecx	#, tmp63
	rep stosq
	movq	%rsp, %rbp	#,
LCFI9:
	movq	%rdx, %rax	# range, <result>
	leave
	ret
LFE9:
	.align 4,0x90
.globl _make_range_count
_make_range_count:
LFB10:
	pushq	%rbp	#
LCFI10:
	leaq	104(,%rdi,8), %rdi	#, tmp63
	movq	%rsp, %rbp	#,
LCFI11:
	call	_malloc	#
	movq	%rax, %rdx	#, tmp64
	movl	$13, %ecx	#, tmp68
	xorl	%eax, %eax	# tmp66
	movq	%rdx, %rdi	# tmp64,
	rep stosq
	movq	%rdx, %rax	# tmp64, <result>
	leave
	ret
LFE10:
	.align 4,0x90
.globl _test_inserts
_test_inserts:
LFB12:
	pushq	%rbp	#
LCFI12:
	testq	%rsi, %rsi	# repeat
	movq	%rsp, %rbp	#,
LCFI13:
	je	L33	#,
	movq	96(%rdi), %r9	# <variable>.log_count, <variable>.log_count
	xorl	%r8d, %r8d	# i
	movl	$1, %r10d	#, tmp76
	.align 4,0x90
L32:
	leaq	12(%r9), %rdx	#, tmp66
	movl	%r8d, %eax	#, payload$key
	movl	%r8d, %ecx	#, tmp70
	andl	$511, %eax	#, payload$key
	andl	$63, %ecx	#, tmp70
	addq	$1, %r9	#, <variable>.log_count
	movl	%eax, 8(%rdi,%rdx,8)	# payload$key, <variable>.key
	shrl	$6, %eax	#, payload$key
	movl	%r8d, 12(%rdi,%rdx,8)	# i, <variable>.value
	cltq
	movq	%r10, %rdx	# tmp76,
	addq	$1, %r8	#, i
	salq	%cl, %rdx	# tmp70,
	orq	%rdx, (%rdi,%rax,8)	# tmp71, <variable>.masks
	cmpq	%r8, %rsi	# i, repeat
	movq	%r9, 96(%rdi)	# <variable>.log_count, <variable>.log_count
	ja	L32	#,
L33:
	leave
	ret
LFE12:
	.align 4,0x90
.globl _bucket_sort
_bucket_sort:
LFB15:
	pushq	%rbp	#
LCFI14:
	leaq	32(%rdx), %r9	#, ivtmp.229
	movl	$4, %r8d	#, i
	movdqa	LC0(%rip), %xmm3	#, tmp181
	movabsq	$3689348814741910323, %r10	#, tmp267
	movq	%rsp, %rbp	#,
LCFI15:
	pushq	%r14	#
LCFI16:
	movabsq	$1085102592571150095, %r11	#, tmp268
	movq	%rsi, %r14	# max_buckets, max_buckets
	movabsq	$6148914691236517205, %rsi	#, tmp266
	pushq	%r13	#
LCFI17:
	movq	%rdi, %r13	# buckets, buckets
	movq	%rdx, %rdi	# range, range
	pushq	%r12	#
LCFI18:
	pushq	%rbx	#
LCFI19:
	movdqu	(%rdx), %xmm1	#* range, tmp177
	movdqa	%xmm1, %xmm0	# tmp177, tmp179
	movdqu	16(%rdx), %xmm2	#, tmp178
	psrlq	$1, %xmm0	#, tmp179
	pand	%xmm3, %xmm0	# tmp181, tmp179
	psubq	%xmm0, %xmm1	# tmp179, vect_var_.194
	movdqa	%xmm2, %xmm0	# tmp178, tmp182
	psrlq	$1, %xmm0	#, tmp182
	pand	%xmm3, %xmm0	# tmp181, tmp182
	movdqa	%xmm1, %xmm3	# vect_var_.194, vect_var_.200
	psubq	%xmm0, %xmm2	# tmp182, vect_var_.280
	movdqa	LC1(%rip), %xmm0	#, tmp187
	psrlq	$2, %xmm3	#, vect_var_.200
	pand	%xmm0, %xmm1	# tmp187, vect_var_.194
	pand	%xmm0, %xmm3	# tmp187, vect_var_.200
	paddq	%xmm1, %xmm3	# vect_var_.194, vect_var_.200
	movdqa	%xmm2, %xmm1	# vect_var_.280, vect_var_.284
	pand	%xmm0, %xmm2	# tmp187, vect_var_.280
	psrlq	$2, %xmm1	#, vect_var_.284
	pand	%xmm0, %xmm1	# tmp187, vect_var_.284
	paddq	%xmm2, %xmm1	# vect_var_.280, vect_var_.284
	movdqa	%xmm3, %xmm2	# vect_var_.200, vect_var_.203
	movdqa	%xmm1, %xmm0	# vect_var_.284, vect_var_.287
	psrlq	$4, %xmm2	#, vect_var_.203
	paddq	%xmm3, %xmm2	# vect_var_.200, vect_var_.203
	movdqa	LC2(%rip), %xmm3	#, tmp197
	psrlq	$4, %xmm0	#, vect_var_.287
	paddq	%xmm1, %xmm0	# vect_var_.284, vect_var_.287
	pand	%xmm3, %xmm2	# tmp197, vect_var_.203
	pand	%xmm3, %xmm0	# tmp197, vect_var_.287
	movdqa	%xmm2, %xmm1	# vect_var_.203, vect_var_.206
	psrlq	$8, %xmm1	#, vect_var_.206
	paddq	%xmm2, %xmm1	# vect_var_.203, vect_var_.206
	movdqa	%xmm0, %xmm2	# vect_var_.287, vect_var_.289
	movdqa	%xmm1, %xmm3	# vect_var_.206, vect_var_.208
	psrlq	$8, %xmm2	#, vect_var_.289
	paddq	%xmm0, %xmm2	# vect_var_.287, vect_var_.289
	movdqa	%xmm2, %xmm4	# vect_var_.289, vect_var_.291
	psrlq	$16, %xmm3	#, vect_var_.208
	paddq	%xmm1, %xmm3	# vect_var_.206, vect_var_.208
	movdqa	%xmm3, %xmm0	# vect_var_.208, tmp207
	psrlq	$16, %xmm4	#, vect_var_.291
	paddq	%xmm2, %xmm4	# vect_var_.289, vect_var_.291
	movdqa	%xmm4, %xmm1	# vect_var_.291, tmp209
	psrlq	$32, %xmm0	#, tmp207
	paddq	%xmm3, %xmm0	# vect_var_.208, tmp207
	movdqa	%xmm0, %xmm2	#, tmp212
	psrlq	$32, %xmm1	#, tmp209
	paddq	%xmm4, %xmm1	# vect_var_.291, tmp209
	punpckldq	%xmm1, %xmm0	# tmp209, tmp213
	punpckhdq	%xmm1, %xmm2	# tmp209, tmp212
	punpckldq	%xmm2, %xmm0	# tmp212, tmp213
	pand	LC3(%rip), %xmm0	#, tmp213
	movdqa	%xmm0, -64(%rbp)	# tmp213,
	.align 4,0x90
L36:
	movq	(%r9), %rdx	# <variable>.masks, D.3068
	addq	$8, %r9	#, ivtmp.229
	movq	%rdx, %rax	# D.3068, tmp218
	shrq	%rax	# tmp218
	andq	%rsi, %rax	# tmp266, tmp218
	subq	%rax, %rdx	# tmp218, x.275
	movq	%rdx, %rcx	# x.275, tmp221
	andq	%r10, %rdx	# tmp267, x.275
	shrq	$2, %rcx	#, tmp221
	andq	%r10, %rcx	# tmp267, tmp221
	addq	%rdx, %rcx	# x.275, x.273
	movq	%rcx, %rax	# x.273, tmp224
	shrq	$4, %rax	#, tmp224
	addq	%rcx, %rax	# x.273, x.272
	andq	%r11, %rax	# tmp268, x.272
	movq	%rax, %rdx	# x.272, D.3475
	shrq	$8, %rdx	#, D.3475
	addq	%rax, %rdx	# x.272, x
	movq	%rdx, %rcx	# x, D.3476
	shrq	$16, %rcx	#, D.3476
	addq	%rdx, %rcx	# x, x.271
	movslq	%r8d,%rdx	# i, i
	addl	$1, %r8d	#, i
	movq	%rcx, %rax	# x.271, D.3477
	shrq	$32, %rax	#, D.3477
	addq	%rcx, %rax	# x.271, tmp227
	andl	$127, %eax	#, tmp228
	cmpl	$7, %r8d	#, i
	movl	%eax, -64(%rbp,%rdx,4)	# tmp228, counts
	jne	L36	#,
	movl	$0, 64(%rdi)	#, <variable>.partial_sums
	movl	-64(%rbp), %eax	# counts, temp.265
	xorl	%r11d, %r11d	# nbucket
	xorl	%ebx, %ebx	# ivtmp.261
	movl	$1, %r12d	#, tmp264
	movl	%eax, 68(%rdi)	# temp.265, <variable>.partial_sums
	addl	-60(%rbp), %eax	# counts, acc
	movl	%eax, 72(%rdi)	# acc, <variable>.partial_sums
	addl	-56(%rbp), %eax	# counts, acc.295
	movl	%eax, 76(%rdi)	# acc.295, <variable>.partial_sums
	addl	-52(%rbp), %eax	# counts, acc.297
	movl	%eax, 80(%rdi)	# acc.297, <variable>.partial_sums
	addl	-48(%rbp), %eax	# counts, acc.299
	movl	%eax, 84(%rdi)	# acc.299, <variable>.partial_sums
	addl	-44(%rbp), %eax	# counts, acc.301
	movl	%eax, 88(%rdi)	# acc.301, <variable>.partial_sums
	addl	-40(%rbp), %eax	# counts, acc.301
	movl	%eax, 92(%rdi)	# acc.301, <variable>.partial_sums
L43:
	movq	(%rdi,%rbx), %r10	# <variable>.masks, mask
	leal	0(,%rbx,8), %esi	#, base
	testq	%r10, %r10	# mask
	je	L37	#,
	movslq	%r11d,%r9	# nbucket, ivtmp.274
	cmpq	%r9, %r14	# ivtmp.274, max_buckets
	jb	L38	#,
	leal	1(%r11), %eax	#, tmp232
	movslq	%eax,%r8	# tmp232, ivtmp.276
	jmp	L40	#
	.align 4,0x90
L42:
	cmpq	%r8, %r14	# ivtmp.276, max_buckets
	leaq	1(%r8), %rax	#, ivtmp.242
	jb	L38	#,
	movq	%r8, %r9	# ivtmp.276, ivtmp.274
	movq	%rax, %r8	# ivtmp.242, ivtmp.276
L40:
	bsfq	%r10, %rcx	# mask, offset
	movq	%r12, %rdx	# tmp264, tmp234
	leaq	(%r13,%r9,8), %rax	#, D.3083
	addl	$1, %r11d	#, nbucket
	salq	%cl, %rdx	# offset, tmp234
	addl	%esi, %ecx	# base, tmp237
	xorq	%rdx, %r10	# tmp234, mask
	movl	$0, 4(%rax)	#, <variable>.value
	movl	%ecx, (%rax)	# tmp237, <variable>.key
	jne	L42	#,
L37:
	addq	$8, %rbx	#, ivtmp.261
	cmpq	$64, %rbx	#, ivtmp.261
	jne	L43	#,
	movq	96(%rdi), %r10	# <variable>.log_count, log_count
	testq	%r10, %r10	# log_count
	je	L41	#,
	xorl	%r8d, %r8d	# i
	movl	$1, %r12d	#, tmp259
	movabsq	$6148914691236517205, %r14	#, tmp260
	movabsq	$3689348814741910323, %r9	#, tmp261
	movabsq	$1085102592571150095, %rbx	#, tmp262
	.align 4,0x90
L44:
	movl	104(%rdi,%r8,8), %ecx	# <variable>.key, key
	movq	%r12, %rax	# tmp259,
	movl	%ecx, %esi	# key, tmp238
	andl	$63, %ecx	#, key
	salq	%cl, %rax	# key,
	shrl	$6, %esi	#, tmp238
	movslq	%esi,%rsi	# tmp238, D.3091
	movq	%rax, %rcx	#, tmp240
	subq	$1, %rcx	#, D.3099
	andq	(%rdi,%rsi,8), %rcx	# <variable>.masks, D.3099
	movq	%rcx, %rax	# D.3099, tmp243
	shrq	%rax	# tmp243
	andq	%r14, %rax	# tmp260, tmp243
	subq	%rax, %rcx	# tmp243, x
	movq	%rcx, %rdx	# x, tmp246
	andq	%r9, %rcx	# tmp261, x
	shrq	$2, %rdx	#, tmp246
	andq	%r9, %rdx	# tmp261, tmp246
	addq	%rcx, %rdx	# x, x.266
	movq	%rdx, %rax	# x.266, tmp249
	shrq	$4, %rax	#, tmp249
	addq	%rdx, %rax	# x.266, x.267
	andq	%rbx, %rax	# tmp262, x.267
	movq	%rax, %rdx	# x.267, D.3489
	shrq	$8, %rdx	#, D.3489
	addq	%rax, %rdx	# x.267, x.268
	movq	%rdx, %rcx	# x.268, D.3490
	shrq	$16, %rcx	#, D.3490
	addq	%rdx, %rcx	# x.268, x.269
	movl	108(%rdi,%r8,8), %edx	# <variable>.value, <variable>.value
	addq	$1, %r8	#, i
	movq	%rcx, %rax	# x.269, D.3491
	shrq	$32, %rax	#, D.3491
	addq	%rcx, %rax	# x.269, tmp251
	andl	$127, %eax	#, tmp252
	addl	64(%rdi,%rsi,4), %eax	# <variable>.partial_sums, tmp252
	cltq
	addl	%edx, 4(%r13,%rax,8)	# <variable>.value, <variable>.value
	cmpq	%r8, %r10	# i, log_count
	ja	L44	#,
	popq	%rbx	#
	popq	%r12	#
	popq	%r13	#
	movl	%r11d, %eax	# nbucket, <result>
	popq	%r14	#
	leave
	ret
	.align 4,0x90
L38:
	negl	%r11d	# nbucket
L41:
	popq	%rbx	#
	popq	%r12	#
	popq	%r13	#
	movl	%r11d, %eax	# nbucket, <result>
	popq	%r14	#
	leave
	ret
LFE15:
	.cstring
LC4:
	.ascii "ninserts == nbuckets\0"
LC5:
	.ascii "insert-and-sort.c\0"
LC6:
	.ascii "MAX_INDEX == nbuckets\0"
LC7:
	.ascii "%i -> %i; expected %i -> %i\12\0"
LC8:
	.ascii "!delta_p\0"
	.text
	.align 4,0x90
.globl _sanity_check
_sanity_check:
LFB16:
	pushq	%rbp	#
LCFI20:
	xorl	%eax, %eax	# tmp71
	movq	%rdi, %r8	# range, range
	movl	$512, %ecx	#, tmp73
	movq	%rsp, %rbp	#,
LCFI21:
	pushq	%r14	#
LCFI22:
	leaq	-4128(%rbp), %r14	#, tmp84
	pushq	%r13	#
LCFI23:
	movq	%r14, %rdi	# tmp84,
	pushq	%r12	#
LCFI24:
	pushq	%rbx	#
LCFI25:
	movq	%rsi, %rbx	# ninserts, ninserts
	subq	$8192, %rsp	#,
LCFI26:
	testq	%rsi, %rsi	# ninserts
	rep stosq
	je	L52	#,
	xorl	%edx, %edx	# i
	.align 4,0x90
L53:
	movq	%rdx, %rax	# i, D.3124
	andl	$511, %eax	#, D.3124
	addq	%rdx, -4128(%rbp,%rax,8)	# i, counts
	addq	$1, %rdx	#, i
	cmpq	%rdx, %rbx	# i, ninserts
	ja	L53	#,
	leaq	-8224(%rbp), %r12	#, tmp83
	movq	%r8, %rdx	# range, range
	movl	$512, %esi	#,
	movq	%r12, %rdi	# tmp83,
	call	_bucket_sort	#
	cmpq	$511, %rbx	#, ninserts
	movslq	%eax,%r13	# D.3127, nbuckets
	jbe	L63	#,
	cmpq	$512, %r13	#, nbuckets
	jne	L67	#,
L57:
	xorl	%edx, %edx	# delta_p
	xorl	%ebx, %ebx	# i
	.align 4,0x90
L62:
	movl	(%r12,%rbx,8), %esi	# <variable>.key, D.3135
	mov	%esi, %eax	# D.3135, D.3136
	cmpq	%rbx, %rax	# i, D.3136
	jne	L60	#,
	movslq	-8220(%rbp,%rbx,8),%rax	# <variable>.value, <variable>.value
	cmpq	%rax, -4128(%rbp,%rbx,8)	# <variable>.value, counts
	je	L61	#,
L60:
	movl	4(%r12,%rbx,8), %edx	# <variable>.value, <variable>.value
	movq	(%r14,%rbx,8), %r8	# counts,
	leaq	LC7(%rip), %rdi	#,
	movq	%rbx, %rcx	# i, i
	xorl	%eax, %eax	#
	call	_printf	#
	movl	$1, %edx	#, delta_p
L61:
	addq	$1, %rbx	#, i
	cmpq	%rbx, %r13	# i, nbuckets
	ja	L62	#,
	testl	%edx, %edx	# delta_p
	jne	L68	#,
L64:
	addq	$8192, %rsp	#,
	popq	%rbx	#
	popq	%r12	#
	popq	%r13	#
	popq	%r14	#
	leave
	ret
L52:
	leaq	-8224(%rbp), %r12	#, tmp83
	movq	%r8, %rdx	# range, range
	movl	$512, %esi	#,
	movq	%r12, %rdi	# tmp83,
	call	_bucket_sort	#
	movslq	%eax,%r13	# temp.332, nbuckets
L63:
	cmpq	%r13, %rbx	# nbuckets, ninserts
	jne	L69	#,
	testq	%r13, %r13	# nbuckets
	jne	L57	#,
	jmp	L64	#
L68:
	leaq	LC8(%rip), %rcx	#,
	leaq	LC5(%rip), %rsi	#,
	leaq	___func__.3118(%rip), %rdi	#,
	movl	$273, %edx	#,
	call	___assert_rtn	#
L69:
	leaq	LC4(%rip), %rcx	#,
	leaq	LC5(%rip), %rsi	#,
	leaq	___func__.3118(%rip), %rdi	#,
	movl	$260, %edx	#,
	call	___assert_rtn	#
L67:
	leaq	LC6(%rip), %rcx	#,
	leaq	LC5(%rip), %rsi	#,
	leaq	___func__.3118(%rip), %rdi	#,
	movl	$262, %edx	#,
	call	___assert_rtn	#
LFE16:
	.cstring
LC9:
	.ascii "reads\0"
LC10:
	.ascii "inserts: %f %f (%lu)\12\0"
LC11:
	.ascii "sort: %f %f (%lu)\12\0"
	.text
	.align 4,0x90
.globl _main
_main:
LFB17:
	pushq	%rbp	#
LCFI27:
	movq	%rsp, %rbp	#,
LCFI28:
	pushq	%r15	#
LCFI29:
	pushq	%r14	#
LCFI30:
	pushq	%r13	#
LCFI31:
	pushq	%r12	#
LCFI32:
	movq	%rsi, %r12	# argv, argv
	pushq	%rbx	#
LCFI33:
	movl	%edi, %ebx	# argc, argc
	subq	$40, %rsp	#,
LCFI34:
	cmpl	$1, %edi	#, argc
	jle	L71	#,
	movq	8(%rsi), %rdi	#, tmp98
	call	_atoi	#
	cmpl	$2, %ebx	#, argc
	movslq	%eax,%r13	# D.3167, ninserts
	leaq	104(,%r13,8), %rdi	#, prephitmp.386
	je	L73	#,
	movq	16(%r12), %rdi	#, tmp100
	call	_atoi	#
	cmpl	$3, %ebx	#, argc
	cltq
	leaq	104(,%r13,8), %rdi	#, prephitmp.386
	movq	%rax, -72(%rbp)	#, repetitions
	je	L75	#,
	movq	24(%r12), %rdi	#, D.3935
	leaq	LC9(%rip), %rsi	#, tmp102
	movl	$6, %ecx	#, tmp106
	repz cmpsb
	leaq	104(,%r13,8), %rdi	#, tmp110
	seta	%bl	#, tmp107
	setb	%al	#, tmp108
	subb	%al, %bl	# tmp108, test_write
	movsbl	%bl,%ebx	# test_write, test_write
	call	_malloc	#
	movq	%rax, %r14	#, tmp111
	movq	%rax, %r12	# tmp111, range
	movl	$13, %ecx	#, tmp115
	xorl	%eax, %eax	# tmp113
	movq	%r14, %rdi	# tmp111, range
	testl	%ebx, %ebx	# test_write
	rep stosq
	jne	L77	#,
	testq	%r13, %r13	# ninserts
	je	L79	#,
	movq	96(%r14), %rdi	# <variable>.log_count, <variable>.log_count
	xorl	%esi, %esi	# i
	movl	$1, %r8d	#, tmp194
	.align 4,0x90
L92:
	leaq	12(%rdi), %rdx	#, tmp149
	movl	%esi, %eax	#, payload$key
	movl	%esi, %ecx	#, tmp153
	andl	$511, %eax	#, payload$key
	andl	$63, %ecx	#, tmp153
	addq	$1, %rdi	#, <variable>.log_count
	movl	%eax, 8(%r14,%rdx,8)	# payload$key, <variable>.key
	shrl	$6, %eax	#, payload$key
	movl	%esi, 12(%r14,%rdx,8)	# i, <variable>.value
	cltq
	movq	%r8, %rdx	# tmp194,
	addq	$1, %rsi	#, i
	salq	%cl, %rdx	# tmp153,
	orq	%rdx, (%r14,%rax,8)	# tmp154, <variable>.masks
	cmpq	%rsi, %r13	# i, ninserts
	movq	%rdi, 96(%r14)	# <variable>.log_count, <variable>.log_count
	ja	L92	#,
L79:
	movq	%r13, %rsi	# ninserts, ninserts
	movq	%r14, %rdi	# tmp111, range
	call	_sanity_check	#
	movl	$4096, %edi	#,
	call	_malloc	#
	cmpq	$0, -72(%rbp)	#, repetitions
	movq	%rax, -80(%rbp)	#,
	je	L91	#,
	testq	%r13, %r13	# ninserts
	js	L93	#,
	cvtsi2sdq	%r13, %xmm0	# ninserts,
	movsd	%xmm0, -56(%rbp)	#, pretmp.394
L94:
	xorl	%ebx, %ebx	# i
	jmp	L99	#
	.align 4,0x90
L106:
	cvtsi2sdq	%rdx, %xmm0	# tmp165, tmp166
L96:
	movq	%r12, %rdx	#, tmp171
	mov	%r15d, %eax	# a, a
	salq	$32, %rdx	#, tmp171
	orq	%rax, %rdx	# a, tmp173
	js	L97	#,
	cvtsi2sdq	%rdx, %xmm1	# tmp173, tmp174
L98:
	subsd	%xmm1, %xmm0	# tmp174, time
	leaq	LC11(%rip), %rdi	#,
	movq	%r13, %rsi	# ninserts, ninserts
	movl	$2, %eax	#,
	addq	$1, %rbx	#, i
	movapd	%xmm0, %xmm1	# time,
	divsd	-56(%rbp), %xmm1	# pretmp.394,
	call	_printf	#
	cmpq	%rbx, -72(%rbp)	# i, repetitions
	jbe	L91	#,
L99:
# 226 "cycle.h" 1
	rdtsc
# 0 "" 2
	movq	-80(%rbp), %rdi	#, data
	movl	%edx, %r12d	#, d
	movl	$512, %esi	#,
	movq	%r14, %rdx	# tmp111, range
	movl	%eax, %r15d	#, a
	call	_bucket_sort	#
# 226 "cycle.h" 1
	rdtsc
# 0 "" 2
	salq	$32, %rdx	#, tmp163
	mov	%eax, %eax	# a, a
	orq	%rax, %rdx	# a, tmp165
	jns	L106	#,
	movq	%rdx, %rax	# tmp165, tmp168
	andl	$1, %edx	#, tmp165
	shrq	%rax	# tmp168
	orq	%rdx, %rax	# tmp165, tmp168
	cvtsi2sdq	%rax, %xmm0	# tmp168, tmp166
	addsd	%xmm0, %xmm0	# tmp166, tmp166
	jmp	L96	#
L71:
	movl	$255, %r13d	#, ninserts
	movl	$2144, %edi	#, prephitmp.386
L73:
	movq	$1000, -72(%rbp)	#, repetitions
L75:
	call	_malloc	#
	movq	%rax, %r12	#, range
	movl	$13, %ecx	#, tmp184
	xorl	%eax, %eax	# tmp182
	movq	%r12, %rdi	# range, range
	rep stosq
L77:
	cmpq	$0, -72(%rbp)	#, repetitions
	je	L80	#,
	testq	%r13, %r13	# ninserts
	js	L81	#,
	cvtsi2sdq	%r13, %xmm0	# ninserts,
	movsd	%xmm0, -64(%rbp)	#, pretmp.387
L82:
	xorl	%ebx, %ebx	# i
	movl	$13, %r15d	#, tmp189
	movl	$1, %r14d	#, tmp190
	.align 4,0x90
L90:
	movq	%r12, %rdi	# range, range
	movq	%r15, %rcx	# tmp189, tmp122
	xorl	%eax, %eax	#
	rep stosq
# 226 "cycle.h" 1
	rdtsc
# 0 "" 2
	testq	%r13, %r13	# ninserts
	movl	%edx, %r8d	#, d
	movl	%eax, %r9d	#, a
	je	L84	#,
	movq	96(%r12), %rdi	# <variable>.log_count, <variable>.log_count
	xorl	%esi, %esi	# i
	.align 4,0x90
L85:
	leaq	12(%rdi), %rdx	#, tmp123
	movl	%esi, %eax	#, payload$key
	movl	%esi, %ecx	#, tmp127
	andl	$511, %eax	#, payload$key
	andl	$63, %ecx	#, tmp127
	addq	$1, %rdi	#, <variable>.log_count
	movl	%eax, 8(%r12,%rdx,8)	# payload$key, <variable>.key
	shrl	$6, %eax	#, payload$key
	movl	%esi, 12(%r12,%rdx,8)	# i, <variable>.value
	cltq
	movq	%r14, %rdx	# tmp190,
	addq	$1, %rsi	#, i
	salq	%cl, %rdx	# tmp127,
	orq	%rdx, (%r12,%rax,8)	# tmp128, <variable>.masks
	cmpq	%r13, %rsi	# ninserts, i
	movq	%rdi, 96(%r12)	# <variable>.log_count, <variable>.log_count
	jb	L85	#,
L84:
# 226 "cycle.h" 1
	rdtsc
# 0 "" 2
	salq	$32, %rdx	#, tmp133
	mov	%eax, %eax	# a, a
	orq	%rax, %rdx	# a, tmp135
	js	L86	#,
	cvtsi2sdq	%rdx, %xmm0	# tmp135, tmp136
L87:
	movq	%r8, %rdx	#, tmp141
	mov	%r9d, %eax	# a, a
	salq	$32, %rdx	#, tmp141
	orq	%rax, %rdx	# a, tmp143
	js	L88	#,
	cvtsi2sdq	%rdx, %xmm1	# tmp143, tmp144
L89:
	subsd	%xmm1, %xmm0	# tmp144, time
	leaq	LC10(%rip), %rdi	#,
	movq	%r13, %rsi	# ninserts, ninserts
	movl	$2, %eax	#,
	addq	$1, %rbx	#, i
	movapd	%xmm0, %xmm1	# time,
	divsd	-64(%rbp), %xmm1	# pretmp.387,
	call	_printf	#
	cmpq	-72(%rbp), %rbx	# repetitions, i
	jb	L90	#,
L80:
	movq	%r13, %rsi	# ninserts, ninserts
	movq	%r12, %rdi	# range, range
	call	_sanity_check	#
L91:
	addq	$40, %rsp	#,
	xorl	%eax, %eax	# <result>
	popq	%rbx	#
	popq	%r12	#
	popq	%r13	#
	popq	%r14	#
	popq	%r15	#
	leave
	ret
	.align 4,0x90
L88:
	movq	%rdx, %rax	# tmp143, tmp146
	andl	$1, %edx	#, tmp143
	shrq	%rax	# tmp146
	orq	%rdx, %rax	# tmp143, tmp146
	cvtsi2sdq	%rax, %xmm1	# tmp146, tmp144
	addsd	%xmm1, %xmm1	# tmp144, tmp144
	jmp	L89	#
	.align 4,0x90
L86:
	movq	%rdx, %rax	# tmp135, tmp138
	andl	$1, %edx	#, tmp135
	shrq	%rax	# tmp138
	orq	%rdx, %rax	# tmp135, tmp138
	cvtsi2sdq	%rax, %xmm0	# tmp138, tmp136
	addsd	%xmm0, %xmm0	# tmp136, tmp136
	jmp	L87	#
L97:
	movq	%rdx, %rax	# tmp173, tmp176
	andl	$1, %edx	#, tmp173
	shrq	%rax	# tmp176
	orq	%rdx, %rax	# tmp173, tmp176
	cvtsi2sdq	%rax, %xmm1	# tmp176, tmp174
	addsd	%xmm1, %xmm1	# tmp174, tmp174
	jmp	L98	#
L81:
	movq	%r13, %rax	# ninserts, tmp117
	movq	%r13, %rdx	# ninserts, tmp118
	shrq	%rax	# tmp117
	andl	$1, %edx	#, tmp118
	orq	%rdx, %rax	# tmp118, tmp117
	cvtsi2sdq	%rax, %xmm0	# tmp117,
	addsd	%xmm0, %xmm0	#,
	movsd	%xmm0, -64(%rbp)	#, pretmp.387
	jmp	L82	#
L93:
	movq	%r13, %rax	# ninserts, tmp160
	movq	%r13, %rdx	# ninserts, tmp161
	shrq	%rax	# tmp160
	andl	$1, %edx	#, tmp161
	orq	%rdx, %rax	# tmp161, tmp160
	cvtsi2sdq	%rax, %xmm0	# tmp160,
	addsd	%xmm0, %xmm0	#,
	movsd	%xmm0, -56(%rbp)	#, pretmp.394
	jmp	L94	#
LFE17:
	.const
___func__.3118:
	.ascii "sanity_check\0"
	.literal16
	.align 4
LC0:
	.quad	6148914691236517205
	.quad	6148914691236517205
	.align 4
LC1:
	.quad	3689348814741910323
	.quad	3689348814741910323
	.align 4
LC2:
	.quad	1085102592571150095
	.quad	1085102592571150095
	.align 4
LC3:
	.long	127
	.long	127
	.long	127
	.long	127
	.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EH_frame1:
	.set L$set$0,LECIE1-LSCIE1
	.long L$set$0
LSCIE1:
	.long	0x0
	.byte	0x1
	.ascii "zR\0"
	.byte	0x1
	.byte	0x78
	.byte	0x10
	.byte	0x1
	.byte	0x10
	.byte	0xc
	.byte	0x7
	.byte	0x8
	.byte	0x90
	.byte	0x1
	.align 3
LECIE1:
.globl _popcount.eh
_popcount.eh:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB8-.
	.set L$set$2,LFE8-LFB8
	.quad L$set$2
	.byte	0x0
	.byte	0x4
	.set L$set$3,LCFI0-LFB8
	.long L$set$3
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$4,LCFI1-LCFI0
	.long L$set$4
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE1:
.globl _insert_payload.eh
_insert_payload.eh:
LSFDE3:
	.set L$set$5,LEFDE3-LASFDE3
	.long L$set$5
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB11-.
	.set L$set$6,LFE11-LFB11
	.quad L$set$6
	.byte	0x0
	.byte	0x4
	.set L$set$7,LCFI2-LFB11
	.long L$set$7
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$8,LCFI3-LCFI2
	.long L$set$8
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE3:
.globl _test_dummy.eh
_test_dummy.eh:
LSFDE5:
	.set L$set$9,LEFDE5-LASFDE5
	.long L$set$9
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB13-.
	.set L$set$10,LFE13-LFB13
	.quad L$set$10
	.byte	0x0
	.byte	0x4
	.set L$set$11,LCFI4-LFB13
	.long L$set$11
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$12,LCFI5-LCFI4
	.long L$set$12
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE5:
.globl _test_insertion.eh
_test_insertion.eh:
LSFDE7:
	.set L$set$13,LEFDE7-LASFDE7
	.long L$set$13
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB14-.
	.set L$set$14,LFE14-LFB14
	.quad L$set$14
	.byte	0x0
	.byte	0x4
	.set L$set$15,LCFI6-LFB14
	.long L$set$15
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$16,LCFI7-LCFI6
	.long L$set$16
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE7:
.globl _init_range_count.eh
_init_range_count.eh:
LSFDE9:
	.set L$set$17,LEFDE9-LASFDE9
	.long L$set$17
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB9-.
	.set L$set$18,LFE9-LFB9
	.quad L$set$18
	.byte	0x0
	.byte	0x4
	.set L$set$19,LCFI8-LFB9
	.long L$set$19
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$20,LCFI9-LCFI8
	.long L$set$20
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE9:
.globl _make_range_count.eh
_make_range_count.eh:
LSFDE11:
	.set L$set$21,LEFDE11-LASFDE11
	.long L$set$21
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB10-.
	.set L$set$22,LFE10-LFB10
	.quad L$set$22
	.byte	0x0
	.byte	0x4
	.set L$set$23,LCFI10-LFB10
	.long L$set$23
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$24,LCFI11-LCFI10
	.long L$set$24
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE11:
.globl _test_inserts.eh
_test_inserts.eh:
LSFDE13:
	.set L$set$25,LEFDE13-LASFDE13
	.long L$set$25
LASFDE13:
	.long	LASFDE13-EH_frame1
	.quad	LFB12-.
	.set L$set$26,LFE12-LFB12
	.quad L$set$26
	.byte	0x0
	.byte	0x4
	.set L$set$27,LCFI12-LFB12
	.long L$set$27
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$28,LCFI13-LCFI12
	.long L$set$28
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE13:
.globl _bucket_sort.eh
_bucket_sort.eh:
LSFDE15:
	.set L$set$29,LEFDE15-LASFDE15
	.long L$set$29
LASFDE15:
	.long	LASFDE15-EH_frame1
	.quad	LFB15-.
	.set L$set$30,LFE15-LFB15
	.quad L$set$30
	.byte	0x0
	.byte	0x4
	.set L$set$31,LCFI14-LFB15
	.long L$set$31
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$32,LCFI15-LCFI14
	.long L$set$32
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$33,LCFI16-LCFI15
	.long L$set$33
	.byte	0x8e
	.byte	0x3
	.byte	0x4
	.set L$set$34,LCFI17-LCFI16
	.long L$set$34
	.byte	0x8d
	.byte	0x4
	.byte	0x4
	.set L$set$35,LCFI19-LCFI17
	.long L$set$35
	.byte	0x83
	.byte	0x6
	.byte	0x8c
	.byte	0x5
	.align 3
LEFDE15:
.globl _sanity_check.eh
_sanity_check.eh:
LSFDE17:
	.set L$set$36,LEFDE17-LASFDE17
	.long L$set$36
LASFDE17:
	.long	LASFDE17-EH_frame1
	.quad	LFB16-.
	.set L$set$37,LFE16-LFB16
	.quad L$set$37
	.byte	0x0
	.byte	0x4
	.set L$set$38,LCFI20-LFB16
	.long L$set$38
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$39,LCFI21-LCFI20
	.long L$set$39
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$40,LCFI22-LCFI21
	.long L$set$40
	.byte	0x8e
	.byte	0x3
	.byte	0x4
	.set L$set$41,LCFI25-LCFI22
	.long L$set$41
	.byte	0x83
	.byte	0x6
	.byte	0x8c
	.byte	0x5
	.byte	0x8d
	.byte	0x4
	.align 3
LEFDE17:
.globl _main.eh
_main.eh:
LSFDE19:
	.set L$set$42,LEFDE19-LASFDE19
	.long L$set$42
LASFDE19:
	.long	LASFDE19-EH_frame1
	.quad	LFB17-.
	.set L$set$43,LFE17-LFB17
	.quad L$set$43
	.byte	0x0
	.byte	0x4
	.set L$set$44,LCFI27-LFB17
	.long L$set$44
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$45,LCFI28-LCFI27
	.long L$set$45
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$46,LCFI32-LCFI28
	.long L$set$46
	.byte	0x8c
	.byte	0x6
	.byte	0x8d
	.byte	0x5
	.byte	0x8e
	.byte	0x4
	.byte	0x8f
	.byte	0x3
	.byte	0x4
	.set L$set$47,LCFI33-LCFI32
	.long L$set$47
	.byte	0x83
	.byte	0x7
	.align 3
LEFDE19:
	.subsections_via_symbols
