    .text
    .align 16, 0x90
    .globl routine
    .type routine,@function
    routine:
    
    pushq %rbx
    pushq %rbp
    pushq %r12
    pushq %r13
    pushq %r14
    pushq %r15
    movsd (%rdi), %xmm0
    xorq %rax, %rax
    .align 16, 0x90
L1:
    movsd (%rdx,%rax,8), %xmm1
    movsd %xmm1, %xmm2
    mulsd (%rsi,%rax,8), %xmm2
    addsd %xmm2, %xmm0
    addq $1, %rax
    cmpq $64, %rax
    jne L1
    movsd %xmm0, (%rdi)
    popq %r15
    popq %r14
    popq %r13
    popq %r12
    popq %rbp
    popq %rbx
    ret
    .size routine,.-routine
