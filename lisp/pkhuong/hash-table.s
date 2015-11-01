# GNU C version 4.0.1 (Apple Inc. build 5484) (i686-apple-darwin9)
#	compiled by GNU C version 4.0.1 (Apple Inc. build 5484).
# GGC heuristics: --param ggc-min-expand=100 --param ggc-min-heapsize=131072
# options passed:  -D__DYNAMIC__ -fPIC -mmacosx-version-min=10.5.5 -m64
# -mtune=generic -march=apple -auxbase -O3 -W -Wall -std=gnu99
# -fverbose-asm
# options enabled:  -fPIC -falign-loops -fargument-alias
# -fasynchronous-unwind-tables -fbranch-count-reg -fcaller-saves -fcommon
# -fcprop-registers -fcrossjumping -fcse-follow-jumps -fcse-skip-blocks
# -fdefer-pop -fdelete-null-pointer-checks -feliminate-unused-debug-types
# -fexpensive-optimizations -ffunction-cse -fgcse -fgcse-after-reload
# -fgcse-lm -fguess-branch-probability -fident -fif-conversion
# -fif-conversion2 -finline-functions -fivopts -fkeep-static-consts
# -fleading-underscore -flocal-alloc -floop-optimize -floop-optimize2
# -fmerge-constants -foptimize-register-move -foptimize-sibling-calls
# -fpeephole -fpeephole2 -freg-struct-return -fregmove -freorder-blocks
# -freorder-functions -frerun-cse-after-loop -frerun-loop-opt
# -fsched-interblock -fsched-spec -fsched-stalled-insns-dep
# -fschedule-insns2 -fsplit-ivs-in-unroller -fstrength-reduce
# -fthread-jumps -ftree-ccp -ftree-ch -ftree-copyrename -ftree-dce
# -ftree-dominator-opts -ftree-dse -ftree-fre -ftree-loop-im
# -ftree-loop-ivcanon -ftree-loop-optimize -ftree-lrs -ftree-pre -ftree-sra
# -ftree-ter -funit-at-a-time -funswitch-loops -funwind-tables
# -fverbose-asm -fzero-initialized-in-bss -m80387 -mhard-float
# -mno-soft-float -mieee-fp -mfp-ret-in-387 -maccumulate-outgoing-args
# -mmmx -msse -msse2 -msse3 -m128bit-long-double -m64 -mtune=generic64
# -march=apple -mmacosx-version-min=10.5.5

# Compiler executable checksum: 493934a1662bc75ab2515dc75b128769

	.text
	.align 4,0x90
.globl _maybe_add_to_descriptor
_maybe_add_to_descriptor:
LFB10:
	pushq	%rbp	#
LCFI0:
	movq	%rsp, %rbp	#,
LCFI1:
	movq	%r12, -32(%rbp)	#,
LCFI2:
	movq	%r13, -24(%rbp)	#,
LCFI3:
	movq	%rdx, %r13	# middle, middle
	movq	%r14, -16(%rbp)	#,
LCFI4:
	movq	%rbx, -40(%rbp)	#,
LCFI5:
	movq	%rcx, %r14	# value, value
	movq	%r15, -8(%rbp)	#,
LCFI6:
	subq	$48, %rsp	#,
LCFI7:
	movq	(%rdi), %rdx	# <variable>.top, D.2925
	movq	%rdi, %r12	# descriptor, descriptor
	movq	%r8, %rcx	# old, old
	cmpq	%rsi, %rdx	# top, D.2925
	je	L14	#,
L2:
	cmpq	$-1, %rdx	#, D.2925
	je	L15	#,
	xorl	%eax, %eax	# D.2931
	cmpq	%rsi, %rdx	# top, D.2925
	je	L16	#,
	movq	-40(%rbp), %rbx	#,
	movq	-32(%rbp), %r12	#,
	movq	-24(%rbp), %r13	#,
	movq	-16(%rbp), %r14	#,
	movq	-8(%rbp), %r15	#,
	leave
	ret
	.align 4,0x90
L16:
	movq	8(%r12), %r15	# <variable>.middle, D.2932
	cmpq	%r13, %r15	# middle, D.2932
	je	L17	#,
	movq	16(%r12), %rbx	# <variable>.value, first_value
	movl	$8, %esi	#,
	movl	$16384, %edi	#,
	call	_calloc	#
	movq	%rax, 24(%r12)	# tmp75, <variable>.values
	movq	%rbx, (%rax,%r15,8)	# first_value,
	movq	%r14, (%rax,%r13,8)	# value,
	movl	$1, %eax	#, D.2931
	movq	-40(%rbp), %rbx	#,
	movq	-32(%rbp), %r12	#,
	movq	-24(%rbp), %r13	#,
	movq	-16(%rbp), %r14	#,
	movq	-8(%rbp), %r15	#,
	leave
	ret
	.align 4,0x90
L14:
	movq	24(%rdi), %rax	# <variable>.values, temp.4
	testq	%rax, %rax	# temp.4
	je	L2	#,
	movq	(%rax,%r13,8), %rax	#* temp.4, tmp72
	movq	%rax, (%r8)	# tmp72,* old
	movq	24(%rdi), %rax	# <variable>.values, <variable>.values
	movq	%r14, (%rax,%r13,8)	# value,* <variable>.values
	movq	-40(%rbp), %rbx	#,
	movl	$1, %eax	#, D.2931
	movq	-32(%rbp), %r12	#,
	movq	-24(%rbp), %r13	#,
	movq	-16(%rbp), %r14	#,
	movq	-8(%rbp), %r15	#,
	leave
	ret
	.align 4,0x90
L15:
	movq	%rsi, (%r12)	# top, <variable>.top
	movq	%r13, 8(%r12)	# middle, <variable>.middle
	movl	$1, %eax	#, D.2931
	movq	%r14, 16(%r12)	# value, <variable>.value
	movq	$0, 24(%r12)	#, <variable>.values
	movq	-40(%rbp), %rbx	#,
	movq	-32(%rbp), %r12	#,
	movq	-24(%rbp), %r13	#,
	movq	-16(%rbp), %r14	#,
	movq	-8(%rbp), %r15	#,
	leave
	ret
	.align 4,0x90
L17:
	movq	16(%r12), %rax	# <variable>.value, <variable>.value
	movq	%rax, (%rcx)	# <variable>.value,* old
	movq	%r14, 16(%r12)	# value, <variable>.value
	movl	$1, %eax	#, D.2931
	movq	-40(%rbp), %rbx	#,
	movq	-32(%rbp), %r12	#,
	movq	-24(%rbp), %r13	#,
	movq	-16(%rbp), %r14	#,
	movq	-8(%rbp), %r15	#,
	leave
	ret
LFE10:
	.align 4,0x90
.globl _init_table
_init_table:
LFB12:
	pushq	%rbp	#
LCFI8:
	movl	$3670016, %edx	#,
	xorl	%esi, %esi	#
	movq	%rsp, %rbp	#,
LCFI9:
	pushq	%rbx	#
LCFI10:
	movq	%rdi, %rbx	# table, table
	subq	$8, %rsp	#,
LCFI11:
	call	_memset	#
	movq	%rbx, %rdi	# table, ivtmp.19
	movl	$65536, %eax	#, ivtmp.10
	.align 4,0x90
L19:
	movq	$-1, (%rdi)	#, <variable>.descriptor.top
	addq	$56, %rdi	#, ivtmp.19
	decq	%rax	# ivtmp.10
	jne	L19	#,
	addq	$8, %rsp	#,
	popq	%rbx	#
	leave
	ret
LFE12:
	.align 4,0x90
.globl _hash_table_get
_hash_table_get:
LFB14:
	movq	%rsi, %r8	# addr, addr
	shrq	$6, %rsi	#, middle
	pushq	%rbp	#
LCFI12:
	shrq	$20, %r8	#, addr
	andl	$16383, %esi	#, middle
	imulq	$7919, %r8, %rax	#, addr, tmp76
	movq	%rsp, %rbp	#,
LCFI13:
	shrq	$13, %rax	#, tmp76
	xorq	%r8, %rax	# addr, tmp76
	andl	$65535, %eax	#, tmp79
	leaq	0(,%rax,8), %rdx	#, tmp81
	salq	$6, %rax	#, tmp79
	subq	%rdx, %rax	# tmp81, tmp79
	leaq	(%rdi,%rax), %rcx	#, bucket
	cmpq	$-1, (%rcx)	#, <variable>.descriptor.top
	je	L25	#,
	cmpq	(%rcx), %r8	# <variable>.top, addr
	jne	L27	#,
	movq	24(%rcx), %rax	# <variable>.values, D.3183
	testq	%rax, %rax	# D.3183
	leaq	(%rax,%rsi,8), %rdx	#, ret
	je	L46	#,
L31:
	testq	%rdx, %rdx	# ret
	je	L27	#,
L33:
	leave
	movq	%rdx, %rax	# ret, <result>
	ret
	.align 4,0x90
L25:
	leave
	xorl	%edx, %edx	# ret
	movq	%rdx, %rax	# ret, <result>
	ret
L46:
	cmpq	8(%rcx), %rsi	# <variable>.middle, middle
	je	L47	#,
	.align 4,0x90
L27:
	movq	32(%rcx), %r9	# <variable>.ndescriptors, D.2994
	testq	%r9, %r9	# D.2994
	je	L25	#,
	movq	48(%rcx), %rcx	# <variable>.descriptors, ivtmp.29
	leaq	0(,%rsi,8), %r10	#, D.3195
	xorl	%edi, %edi	# i
	jmp	L34	#
	.align 4,0x90
L39:
	testq	%rdx, %rdx	# ret
	jne	L33	#,
L35:
	incq	%rdi	# i
	addq	$32, %rcx	#, ivtmp.29
	cmpq	%r9, %rdi	# D.2994, i
	je	L25	#,
L34:
	cmpq	(%rcx), %r8	# <variable>.top, addr
	jne	L35	#,
	movq	24(%rcx), %rax	# <variable>.values, D.3193
	testq	%rax, %rax	# D.3193
	leaq	(%rax,%r10), %rdx	#, ret
	jne	L39	#,
	cmpq	8(%rcx), %rsi	# <variable>.middle, middle
	jne	L35	#,
	leaq	16(%rcx), %rdx	#, ret
	jmp	L39	#
L47:
	leaq	16(%rcx), %rdx	#, ret
	jmp	L31	#
LFE14:
	.align 4,0x90
.globl _make_table
_make_table:
LFB13:
	pushq	%rbp	#
LCFI14:
	movl	$3670016, %edi	#,
	movq	%rsp, %rbp	#,
LCFI15:
	pushq	%rbx	#
LCFI16:
	subq	$8, %rsp	#,
LCFI17:
	call	_malloc	#
	movl	$3670016, %edx	#,
	movq	%rax, %rbx	#, tmp63
	xorl	%esi, %esi	#
	movq	%rax, %rdi	# tmp63, table
	call	_memset	#
	movq	%rbx, %rax	# tmp63, ivtmp.46
	movl	$65536, %edx	#, ivtmp.37
	.align 4,0x90
L49:
	movq	$-1, (%rax)	#, <variable>.descriptor.top
	addq	$56, %rax	#, ivtmp.46
	decq	%rdx	# ivtmp.37
	jne	L49	#,
	movq	%rbx, %rax	# tmp63, <result>
	addq	$8, %rsp	#,
	popq	%rbx	#
	leave
	ret
LFE13:
	.align 4,0x90
.globl _hash_table_set
_hash_table_set:
LFB15:
	pushq	%rbp	#
LCFI18:
	movq	%rsp, %rbp	#,
LCFI19:
	movq	%rbx, -40(%rbp)	#,
LCFI20:
	movq	%rsi, %rbx	# addr, addr
	movq	%r15, -8(%rbp)	#,
LCFI21:
	shrq	$20, %rbx	#, addr
	movq	%rdx, %r15	# value, value
	movq	%r13, -24(%rbp)	#,
LCFI22:
	imulq	$7919, %rbx, %rax	#, addr, tmp93
	movq	%r14, -16(%rbp)	#,
LCFI23:
	movq	%r12, -32(%rbp)	#,
LCFI24:
	subq	$48, %rsp	#,
LCFI25:
	movq	%rsi, %r14	# addr, middle
	shrq	$6, %r14	#, middle
	andl	$16383, %r14d	#, middle
	shrq	$13, %rax	#, tmp93
	xorq	%rbx, %rax	# addr, tmp93
	leaq	0(,%rax,8), %rdx	#, tmp95
	salq	$6, %rax	#, tmp93
	subq	%rdx, %rax	# tmp95, tmp93
	leaq	(%rdi,%rax), %r13	#, bucket
	movq	(%r13), %rax	# <variable>.top, D.3304
	cmpq	%rax, %rbx	# D.3304, addr
	je	L88	#,
L55:
	cmpq	$-1, %rax	#, D.3304
	je	L89	#,
	cmpq	%rax, %rbx	# D.3304, addr
	je	L90	#,
	movq	32(%r13), %rcx	# <variable>.ndescriptors, <variable>.ndescriptors
	testq	%rcx, %rcx	# <variable>.ndescriptors
	jne	L91	#,
	movq	48(%r13), %rdi	# <variable>.descriptors, <variable>.descriptors
L75:
	cmpq	%rcx, 40(%r13)	# <variable>.ndescriptors, <variable>.max_descriptors
	ja	L76	#,
	leaq	(%rcx,%rcx), %r12	#, new_max
	cmpq	$1, %r12	#, new_max
	jbe	L92	#,
	movq	%rcx, %rsi	# <variable>.ndescriptors, prephitmp.52
	salq	$6, %rsi	#, prephitmp.52
L80:
	call	_realloc	#
	movq	32(%r13), %rcx	# <variable>.ndescriptors, <variable>.ndescriptors
	movq	%rax, %rdi	#, <variable>.descriptors
	movq	%rax, 48(%r13)	# <variable>.descriptors, <variable>.descriptors
	movq	%r12, 40(%r13)	# new_max, <variable>.max_descriptors
L76:
	movq	%rcx, %rax	# <variable>.ndescriptors, tmp105
	xorl	%edx, %edx	# ret
	incq	%rcx	# <variable>.ndescriptors
	salq	$5, %rax	#, tmp105
	movq	%rcx, 32(%r13)	# <variable>.ndescriptors, <variable>.ndescriptors
	addq	%rdi, %rax	# <variable>.descriptors, descriptor
	movq	%rbx, (%rax)	# addr, <variable>.top
	movq	%r14, 8(%rax)	# middle, <variable>.middle
	movq	%r15, 16(%rax)	# value, <variable>.value
	movq	$0, 24(%rax)	#, <variable>.values
	jmp	L58	#
	.align 4,0x90
L88:
	movq	24(%r13), %rdx	# <variable>.values, temp.66
	testq	%rdx, %rdx	# temp.66
	je	L55	#,
	leaq	(%rdx,%r14,8), %rax	#, temp.63
	movq	(%rax), %rdx	#* temp.63, ret
	movq	%r15, (%rax)	# value,* temp.63
	.align 4,0x90
L58:
	movq	-40(%rbp), %rbx	#,
	movq	-32(%rbp), %r12	#,
	movq	%rdx, %rax	# ret, <result>
	movq	-24(%rbp), %r13	#,
	movq	-16(%rbp), %r14	#,
	movq	-8(%rbp), %r15	#,
	leave
	ret
	.align 4,0x90
L89:
	xorl	%edx, %edx	# ret
	movq	%rbx, (%r13)	# addr, <variable>.top
	movq	%r14, 8(%r13)	# middle, <variable>.middle
	movq	%r15, 16(%r13)	# value, <variable>.value
	movq	$0, 24(%r13)	#, <variable>.values
	jmp	L58	#
	.align 4,0x90
L91:
	movq	48(%r13), %r12	# <variable>.descriptors, ivtmp.58
	xorl	%edx, %edx	# i
	movq	%r12, %rdi	# ivtmp.58, <variable>.descriptors
	jmp	L65	#
	.align 4,0x90
L66:
	cmpq	$-1, %rax	#, D.3326
	je	L85	#,
	cmpq	%rax, %rbx	# D.3326, addr
	je	L93	#,
	incq	%rdx	# i
	addq	$32, %r12	#, ivtmp.58
	cmpq	%rcx, %rdx	# <variable>.ndescriptors, i
	je	L75	#,
L65:
	movq	(%r12), %rax	# <variable>.top, D.3326
	cmpq	%rax, %rbx	# D.3326, addr
	jne	L66	#,
	movq	24(%r12), %rsi	# <variable>.values, temp.68
	testq	%rsi, %rsi	# temp.68
	je	L66	#,
	movq	(%rsi,%r14,8), %rdx	#* temp.68, ret
	movq	%r15, (%rsi,%r14,8)	# value,* temp.68
	jmp	L58	#
	.align 4,0x90
L90:
	movq	8(%r13), %r12	# <variable>.middle, D.3311
	cmpq	%r12, %r14	# D.3311, middle
	jne	L63	#,
	movq	16(%r13), %rdx	# <variable>.value, ret
	movq	%r15, 16(%r13)	# value, <variable>.value
	jmp	L58	#
L63:
	movq	16(%r13), %rbx	# <variable>.value, first_value
	movl	$8, %esi	#,
	movl	$16384, %edi	#,
	call	_calloc	#
	xorl	%edx, %edx	# ret
	movq	%rax, 24(%r13)	# tmp99, <variable>.values
	movq	%rbx, (%rax,%r12,8)	# first_value,
	movq	%r15, (%rax,%r14,8)	# value,
	jmp	L58	#
L92:
	movl	$2, %r12d	#, new_max
	movl	$64, %esi	#, prephitmp.52
	jmp	L80	#
L85:
	xorl	%edx, %edx	# ret
	movq	%rbx, (%r12)	# addr, <variable>.top
	movq	%r14, 8(%r12)	# middle, <variable>.middle
	movq	%r15, 16(%r12)	# value, <variable>.value
	movq	$0, 24(%r12)	#, <variable>.values
	jmp	L58	#
L93:
	movq	8(%r12), %r13	# <variable>.middle, D.3333
	cmpq	%r13, %r14	# D.3333, middle
	je	L86	#,
	movq	16(%r12), %rbx	# <variable>.value, first_value
	movl	$8, %esi	#,
	movl	$16384, %edi	#,
	call	_calloc	#
	movq	%rax, 24(%r12)	# tmp102, <variable>.values
	xorl	%edx, %edx	# ret
	movq	%rbx, (%rax,%r13,8)	# first_value,
	movq	24(%r12), %rax	# <variable>.values, <variable>.values
	movq	%r15, (%rax,%r14,8)	# value,* <variable>.values
	jmp	L58	#
L86:
	movq	16(%r12), %rdx	# <variable>.value, ret
	movq	%r15, 16(%r12)	# value, <variable>.value
	jmp	L58	#
LFE15:
	.cstring
LC1:
	.ascii "insert: %f\12\0"
LC2:
	.ascii "%lu -> %lu (%lu)\12\0"
LC3:
	.ascii "reads: %f\12\0"
	.literal8
	.align 3
LC0:
	.long	0
	.long	1051721728
	.text
	.align 4,0x90
.globl _main
_main:
LFB16:
	pushq	%rbp	#
LCFI26:
	movl	$8, %esi	#,
	movl	$1048576, %edi	#,
	movq	%rsp, %rbp	#,
LCFI27:
	pushq	%r15	#
LCFI28:
	pushq	%r14	#
LCFI29:
	pushq	%r13	#
LCFI30:
	pushq	%r12	#
LCFI31:
	pushq	%rbx	#
LCFI32:
	subq	$8, %rsp	#,
LCFI33:
	call	_calloc	#
	movl	$3670016, %edi	#,
	movq	%rax, %r13	#, tmp107
	call	_malloc	#
	movl	$3670016, %edx	#,
	movq	%rax, %r14	#, tmp108
	xorl	%esi, %esi	#
	movq	%rax, %rdi	# tmp108, table
	call	_memset	#
	movq	%r14, %rax	# tmp108, ivtmp.139
	xorl	%edx, %edx	# i
	.align 4,0x90
L95:
	incq	%rdx	# i
	movq	$-1, (%rax)	#, <variable>.descriptor.top
	addq	$56, %rax	#, ivtmp.139
	cmpq	$65536, %rdx	#, i
	jne	L95	#,
	movl	$1, %ebx	#, ivtmp.127
	.align 4,0x90
L97:
	movl	$64, %edi	#,
	call	_malloc	#
	movq	%rax, -8(%r13,%rbx,8)	# tmp113,
	incq	%rbx	# ivtmp.127
	cmpq	$1048577, %rbx	#, ivtmp.127
	jne	L97	#,
	rdtsc
	mov	%edx, %edx	# d, d
	mov	%eax, %r12d	# a, begin
	xorl	%ebx, %ebx	# i.140
	salq	$32, %rdx	#, d
	orq	%rdx, %r12	# d, begin
	.align 4,0x90
L99:
	movq	(%r13,%rbx,8), %rsi	#, tmp117
	movq	%rbx, %rdx	# i.140, i.140
	movq	%r14, %rdi	# tmp108, table
	incq	%rbx	# i.140
	call	_hash_table_set	#
	cmpq	$1048576, %rbx	#, i.140
	jne	L99	#,
	rdtsc
	mov	%edx, %edx	# d, d
	mov	%eax, %eax	# a, a
	salq	$32, %rdx	#, d
	orq	%rax, %rdx	# a, tmp121
	js	L101	#,
	cvtsi2sdq	%rdx, %xmm0	# tmp121, tmp122
L102:
	testq	%r12, %r12	# begin
	cvtsi2sdq	%r12, %xmm1	# begin, tmp127
	jns	L104	#,
	movq	%r12, %rax	# begin, tmp130
	andl	$1, %r12d	#, begin
	shrq	%rax	# tmp130
	orq	%r12, %rax	# begin, tmp130
	cvtsi2sdq	%rax, %xmm1	# tmp130, tmp127
	addsd	%xmm1, %xmm1	# tmp127, tmp127
L104:
	subsd	%xmm1, %xmm0	# tmp127, tmp122
	leaq	LC1(%rip), %rdi	#,
	movl	$1, %eax	#,
	mulsd	LC0(%rip), %xmm0	#,
	call	_printf	#
	rdtsc
	mov	%edx, %edx	# d, d
	mov	%eax, %r15d	# a, begin.151
	xorl	%ebx, %ebx	# i
	salq	$32, %rdx	#, d
	orq	%rdx, %r15	# d, begin.151
	.align 4,0x90
L105:
	movq	(%r13,%rbx,8), %rsi	#, addr
	movq	%rsi, %r8	# addr, addr.149
	movq	%rsi, %r10	# addr, middle
	shrq	$20, %r8	#, addr.149
	shrq	$6, %r10	#, middle
	imulq	$7919, %r8, %rax	#, addr.149, tmp139
	andl	$16383, %r10d	#, middle
	shrq	$13, %rax	#, tmp139
	xorq	%r8, %rax	# addr.149, tmp139
	andl	$65535, %eax	#, tmp142
	leaq	0(,%rax,8), %rdx	#, tmp144
	salq	$6, %rax	#, tmp142
	subq	%rdx, %rax	# tmp144, tmp142
	leaq	(%r14,%rax), %rdi	#, bucket
	movq	(%rdi), %r11	# <variable>.descriptor.top, D.3475
	cmpq	$-1, %r11	#, D.3475
	je	L106	#,
	cmpq	%r11, %r8	#, addr.149
	je	L163	#,
L108:
	movq	32(%rdi), %r9	# <variable>.ndescriptors, D.3484
	testq	%r9, %r9	# D.3484
	jne	L164	#,
L106:
	xorl	%edx, %edx	# ret
L114:
	cmpq	%rbx, (%rdx)	# i,* ret
	je	L122	#,
	incq	%r11	# D.3475
	je	L124	#,
	cmpq	(%rdi), %r8	# <variable>.top, addr.149
	je	L165	#,
L126:
	movq	32(%rdi), %r9	# <variable>.ndescriptors, D.3536
	testq	%r9, %r9	# D.3536
	je	L124	#,
	movq	48(%rdi), %rax	# <variable>.descriptors, ivtmp.96
	leaq	0(,%r10,8), %r11	#, D.3564
	xorl	%ecx, %ecx	# i
	jmp	L134	#
	.align 4,0x90
L135:
	incq	%rcx	# i
	addq	$32, %rax	#, ivtmp.96
	cmpq	%rcx, %r9	# i, D.3536
	je	L124	#,
L134:
	cmpq	(%rax), %r8	# <variable>.top, addr.149
	jne	L135	#,
	movq	24(%rax), %rdx	# <variable>.values, D.3562
	testq	%rdx, %rdx	# D.3562
	je	L137	#,
	addq	%r11, %rdx	# D.3564, ret
L139:
	testq	%rdx, %rdx	# ret
	je	L135	#,
	.align 4,0x90
L133:
	leaq	LC2(%rip), %rdi	#,
	movq	%rbx, %rcx	# i, i
	xorl	%eax, %eax	#
	call	_printf	#
L122:
	incq	%rbx	# i
	cmpq	$1048576, %rbx	#, i
	jne	L105	#,
	rdtsc
	mov	%edx, %edx	# d, d
	mov	%eax, %eax	# a, a
	salq	$32, %rdx	#, d
	orq	%rax, %rdx	# a, tmp152
	js	L143	#,
	cvtsi2sdq	%rdx, %xmm0	# tmp152, tmp153
L144:
	testq	%r15, %r15	# begin.151
	cvtsi2sdq	%r15, %xmm1	# begin.151, tmp158
	jns	L146	#,
	movq	%r15, %rax	# begin.151, tmp161
	andl	$1, %r15d	#, begin.151
	shrq	%rax	# tmp161
	orq	%r15, %rax	# begin.151, tmp161
	cvtsi2sdq	%rax, %xmm1	# tmp161, tmp158
	addsd	%xmm1, %xmm1	# tmp158, tmp158
L146:
	subsd	%xmm1, %xmm0	# tmp158, tmp153
	leaq	LC3(%rip), %rdi	#,
	movl	$1, %eax	#,
	mulsd	LC0(%rip), %xmm0	#,
	call	_printf	#
	addq	$8, %rsp	#,
	xorl	%eax, %eax	# <result>
	popq	%rbx	#
	popq	%r12	#
	popq	%r13	#
	popq	%r14	#
	popq	%r15	#
	leave
	ret
	.align 4,0x90
L164:
	movq	48(%rdi), %rax	# <variable>.descriptors, ivtmp.104
	leaq	0(,%r10,8), %r12	#, D.3512
	xorl	%ecx, %ecx	# i
	jmp	L115	#
	.align 4,0x90
L116:
	incq	%rcx	# i
	addq	$32, %rax	#, ivtmp.104
	cmpq	%r9, %rcx	# D.3484, i
	je	L106	#,
L115:
	cmpq	(%rax), %r8	# <variable>.top, addr.149
	jne	L116	#,
	movq	24(%rax), %rdx	# <variable>.values, D.3510
	testq	%rdx, %rdx	# D.3510
	je	L118	#,
	addq	%r12, %rdx	# D.3512, ret
L120:
	testq	%rdx, %rdx	# ret
	je	L116	#,
	jmp	L114	#
	.align 4,0x90
L124:
	xorl	%edx, %edx	# prephitmp.84
	jmp	L133	#
L163:
	movq	24(%rdi), %rax	# <variable>.values, D.3500
	testq	%rax, %rax	# D.3500
	leaq	(%rax,%r10,8), %rdx	#, ret
	je	L166	#,
L112:
	testq	%rdx, %rdx	# ret
	jne	L114	#,
	jmp	L108	#
L165:
	movq	24(%rdi), %rax	# <variable>.values, D.3552
	testq	%rax, %rax	# D.3552
	leaq	(%rax,%r10,8), %rdx	#, ret.144
	je	L167	#,
L130:
	testq	%rdx, %rdx	# prephitmp.84
	jne	L133	#,
	jmp	L126	#
L118:
	cmpq	8(%rax), %r10	# <variable>.middle, middle
	jne	L116	#,
	leaq	16(%rax), %rdx	#, ret
	jmp	L120	#
L137:
	cmpq	8(%rax), %r10	# <variable>.middle, middle
	jne	L135	#,
	leaq	16(%rax), %rdx	#, ret
	jmp	L139	#
L166:
	cmpq	8(%rdi), %r10	# <variable>.middle, middle
	jne	L108	#,
	leaq	16(%rdi), %rdx	#, ret
	jmp	L112	#
L167:
	cmpq	8(%rdi), %r10	# <variable>.middle, middle
	jne	L126	#,
	leaq	16(%rdi), %rdx	#, ret.144
	jmp	L130	#
L143:
	movq	%rdx, %rax	# tmp152, tmp156
	andl	$1, %edx	#, tmp152
	shrq	%rax	# tmp156
	orq	%rdx, %rax	# tmp152, tmp156
	cvtsi2sdq	%rax, %xmm0	# tmp156, tmp153
	addsd	%xmm0, %xmm0	# tmp153, tmp153
	jmp	L144	#
L101:
	movq	%rdx, %rax	# tmp121, tmp125
	andl	$1, %edx	#, tmp121
	shrq	%rax	# tmp125
	orq	%rdx, %rax	# tmp121, tmp125
	cvtsi2sdq	%rax, %xmm0	# tmp125, tmp122
	addsd	%xmm0, %xmm0	# tmp122, tmp122
	jmp	L102	#
LFE16:
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
	.globl _maybe_add_to_descriptor.eh
_maybe_add_to_descriptor.eh:
LSFDE1:
	.set L$set$1,LEFDE1-LASFDE1
	.long L$set$1
LASFDE1:
	.long	LASFDE1-EH_frame1
	.quad	LFB10-.
	.set L$set$2,LFE10-LFB10
	.quad L$set$2
	.byte	0x0
	.byte	0x4
	.set L$set$3,LCFI0-LFB10
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
	.byte	0x4
	.set L$set$5,LCFI3-LCFI1
	.long L$set$5
	.byte	0x8d
	.byte	0x5
	.byte	0x8c
	.byte	0x6
	.byte	0x4
	.set L$set$6,LCFI5-LCFI3
	.long L$set$6
	.byte	0x83
	.byte	0x7
	.byte	0x8e
	.byte	0x4
	.byte	0x4
	.set L$set$7,LCFI7-LCFI5
	.long L$set$7
	.byte	0x8f
	.byte	0x3
	.align 3
LEFDE1:
	.globl _init_table.eh
_init_table.eh:
LSFDE3:
	.set L$set$8,LEFDE3-LASFDE3
	.long L$set$8
LASFDE3:
	.long	LASFDE3-EH_frame1
	.quad	LFB12-.
	.set L$set$9,LFE12-LFB12
	.quad L$set$9
	.byte	0x0
	.byte	0x4
	.set L$set$10,LCFI8-LFB12
	.long L$set$10
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$11,LCFI9-LCFI8
	.long L$set$11
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$12,LCFI10-LCFI9
	.long L$set$12
	.byte	0x83
	.byte	0x3
	.align 3
LEFDE3:
	.globl _hash_table_get.eh
_hash_table_get.eh:
LSFDE5:
	.set L$set$13,LEFDE5-LASFDE5
	.long L$set$13
LASFDE5:
	.long	LASFDE5-EH_frame1
	.quad	LFB14-.
	.set L$set$14,LFE14-LFB14
	.quad L$set$14
	.byte	0x0
	.byte	0x4
	.set L$set$15,LCFI12-LFB14
	.long L$set$15
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$16,LCFI13-LCFI12
	.long L$set$16
	.byte	0xd
	.byte	0x6
	.align 3
LEFDE5:
	.globl _make_table.eh
_make_table.eh:
LSFDE7:
	.set L$set$17,LEFDE7-LASFDE7
	.long L$set$17
LASFDE7:
	.long	LASFDE7-EH_frame1
	.quad	LFB13-.
	.set L$set$18,LFE13-LFB13
	.quad L$set$18
	.byte	0x0
	.byte	0x4
	.set L$set$19,LCFI14-LFB13
	.long L$set$19
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$20,LCFI15-LCFI14
	.long L$set$20
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$21,LCFI17-LCFI15
	.long L$set$21
	.byte	0x83
	.byte	0x3
	.align 3
LEFDE7:
	.globl _hash_table_set.eh
_hash_table_set.eh:
LSFDE9:
	.set L$set$22,LEFDE9-LASFDE9
	.long L$set$22
LASFDE9:
	.long	LASFDE9-EH_frame1
	.quad	LFB15-.
	.set L$set$23,LFE15-LFB15
	.quad L$set$23
	.byte	0x0
	.byte	0x4
	.set L$set$24,LCFI18-LFB15
	.long L$set$24
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$25,LCFI19-LCFI18
	.long L$set$25
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$26,LCFI20-LCFI19
	.long L$set$26
	.byte	0x83
	.byte	0x7
	.byte	0x4
	.set L$set$27,LCFI21-LCFI20
	.long L$set$27
	.byte	0x8f
	.byte	0x3
	.byte	0x4
	.set L$set$28,LCFI25-LCFI21
	.long L$set$28
	.byte	0x8c
	.byte	0x6
	.byte	0x8e
	.byte	0x4
	.byte	0x8d
	.byte	0x5
	.align 3
LEFDE9:
	.globl _main.eh
_main.eh:
LSFDE11:
	.set L$set$29,LEFDE11-LASFDE11
	.long L$set$29
LASFDE11:
	.long	LASFDE11-EH_frame1
	.quad	LFB16-.
	.set L$set$30,LFE16-LFB16
	.quad L$set$30
	.byte	0x0
	.byte	0x4
	.set L$set$31,LCFI26-LFB16
	.long L$set$31
	.byte	0xe
	.byte	0x10
	.byte	0x86
	.byte	0x2
	.byte	0x4
	.set L$set$32,LCFI27-LCFI26
	.long L$set$32
	.byte	0xd
	.byte	0x6
	.byte	0x4
	.set L$set$33,LCFI33-LCFI27
	.long L$set$33
	.byte	0x83
	.byte	0x7
	.byte	0x8c
	.byte	0x6
	.byte	0x8d
	.byte	0x5
	.byte	0x8e
	.byte	0x4
	.byte	0x8f
	.byte	0x3
	.align 3
LEFDE11:
	.subsections_via_symbols
