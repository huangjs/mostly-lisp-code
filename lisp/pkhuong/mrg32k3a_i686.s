	section .bss

%define s10 asm_s10
%define s11 asm_s11
%define s12 asm_s12
%define s20 asm_s20
%define s21 asm_s21
%define s22 asm_s22


s10:	resd	1
s11:	resd	1
s12:	resd	1
s20:	resd	1
s21:	resd	1
s22:	resd	1

	section .text

global _asm_step
global _asm_init

%define m1		4294967087
%define k1		209		; 2^32 - m1
%define m2		4294944443
%define k2		22853		; 2^32 - m2
%define a12		1403580
%define a13n		810728
%define a21		527612
%define a23n		1370589
%define a13n_m1_lo	4125525144	; \ hi and low 32 bits
%define a13n_m1_hi	810727		; / of a13n*m1
%define a23n_m2_lo	3037667951	; \ hi and low 32 bits
%define a23n_m2_hi	1370581		; / of a23n*m2

_asm_step:				; unsigned asm_step();
	push	esi
	push	edi

	; p1 = a12*s11 - a13n*s10 + a13n*m1
	mov	ecx, [s11]		; Remember s11 for later.
	mov	eax, [s12]		; Shuffle state s11' = s12.
	mov	[s11], eax

	mov	eax, a12		; Compute the first term
	mul	ecx			; edx:eax = a12*s11

	mov	esi, eax		; Save the term into edi:esi
	mov	edi, edx		; for later.

	mov	eax, a13n		; Compute the second term
	mov	edx, [s10]		; a13n*s12
	mov	[s10], ecx		; Shuffle state s10' = s11.
	mul	edx			; edx:eax = a13n*s12

	sub	esi, eax		; Form p1 as the diffence of terms
	sbb	edi, edx		; edi:esi = a12*s11 - a13n*s12

	add	esi, a13n_m1_lo		; Add in a fudge term a13n*m1
	adc	edi, a13n_m1_hi + 1 	; to make p1 non-negative.
					; Fold in an inc of the high 32 bits.
					; Explained below.

	; Our next job is to compute p1 % m1 from the 54 bit number
	; p1 + 2^32 in edi:esi. m1 = 2^32 - 209.
	; 
	; First note that to compute % m1 of any largish number we have
	;
	;	x.2^32 + y = xk + y (mod 2^32 - k)
	;
	; so provided the addition of xk to y doesn't overflow 32
	; bits, we've reduced the problem to computing p1 mod m1 to
	; computing the mod m1 of a 32 bit number.
	; 
	; Secondly, consider a 32 bit number z. Now since m1 is so
	; large, the quotient z/m1 is either 0 or 1 according as z <
	; m1 or m1 <= z.  So with a single conditional subtraction we
	; can compute z % m1 as
	;
	; 	z % m1 = z - (z < m1 ? 0 : m1)
	;
	; Written in terms of k1, that is
	;
	;	z % m1	= z 	- 	(z < 2^32 - k1 ? 0 : 2^32 - k1)
	;		= z 	- 	(z+k1 < 2^32 ? 0 : 2^32 - k1)
	;		= z+k1	-	(z+k1 < 2^32 ? k1 : 2^32)
	;
	; which is a long winded to say: if adding k1 to z overflows 32 bits,
	; the wrapped around result is exactly z % m1; if it didn't then 
	; we were wrong to add k1 to z and z already was the right result.
	; 
	; Putting those two together, we have, setting p1 = x*2^32 + y,
	;
	;  p1 + 2^32 = (x+1).2^32 + y	  (mod m1)
	;	     = (x+1)k1 + y	  (mod m1)
	;	     = x*k1 + k1 + y	  (mod m1)
	;
	; Since x < 2^22 and k1 < 2^8, y < 2^32, x*k1 < 2^30,
	; and thus x*k1 + y < 2^32 + 2^30 < 2^33 - 2*k1 = 2*m1,
	; so to compute x*k1 + y % m1, a single conditional subtraction
	; of m1 suffices. Using the second trick, we can change that
	; into a single conditional subtraction of k1.

	; in use: edi:esi = p1 + 2^32
	mov	eax, k1			; Preload k1.
	mov	ecx, eax		; Compute eax = k1*(x+1)
	mul	edi			;         edx = 0

        add     esi, eax                ; Add term to low bits y of p1.
        cmovnc  edx, ecx                ; If y + k1*x + k1 >= 2^32
        sub     esi, edx                ;  then remove the extra k1.
	mov	[s12], esi		; Shuffle state s12' = p1.

	; scratch: eax, ecx, edx, edi
	; in use: esi = p1 % m1.

	;; p2 = x*2^32 + y
	;; x < 2^21
	mov	ecx, [s22]		; Preload s22 for later shuffling.
	mov	eax, a21
	mul	ecx			; Compute first term of p2: a21*s22

;	xchg	ecx, [s21]		; Shuffle state s21' = s22.
					; and load ecx = s21 for shuffling.

	mov	edi, [s21]
	mov	[s21], ecx
	mov	ecx, edi
	xchg	ecx, eax		; Save first term as edi:ecx	

	mov	edi, edx		;  and setup eax = s21.

;	xchg	eax, [s20]		; Shuffle state s20' = s21; eax = s20.

	mov	edx, [s20]
	mov	[s20], eax
	mov	eax, edx

	mov	edx, a23n		; Compute second term s20*a23n of p2
	mul	edx			;  as edx:eax

	sub	ecx, eax		; Subtract the second term of
	sbb	edi, edx		;  p2 from the first term.

	add	ecx, a23n_m2_lo		; Add in a fudge factor to
	adc	edi, a23n_m2_hi		; make p2 non-negative.

	; in use: esi = p1 % m1
	;	  edi:ecx = p2 = a*2^32 + b

	mov	eax, edi		; Reduce high bits a of p2.
	mov	edi, k2			; Preload k2.
	mul	edi			; edx:eax = a*k2 = p*2^32+q
					;   with p < 8
	add	ecx, eax		; Add in low bits of q to b.
	adc	edx, byte 1		; p += 1 plus carry from prev add.
	mov	eax, edi		; Reduce (p+1)*2^32 to
	mul	edx			; eax = k2*(p+1), edx = 0

	add	ecx, eax		; Add in reduced (p+1)*k2 term, and 
	cmovnc	edx, edi		; take out excess k2 if we didn't ovf.
	sub	ecx, edx		; ecx = p2 % m2; yay!
	mov	[s22], ecx		; Shuffle state s22' = p2 % m2.

	; in use: ecx = p2 % m1; esi = p1 % m1

	; Finally, combine the two as (p1 - p2) % m1
	xor	edx, edx
	mov	edi, m1
	mov	eax, esi		; Prep to return in eax.
	sub	eax, ecx		; eax = p1 - p2
	cmovc	edx, edi		; If it underflows then
	add	eax, edx		;  correct by adding back m1.

	; Get out of dodge.
	pop	edi
	pop	esi
	ret

_asm_init:				; void asm_init(unsigned seed[6]);
	mov	ecx, [esp+4]
	mov	eax, [ecx+4*0]
	mov	[s10], eax
	mov	eax, [ecx+4*1]
	mov	[s11], eax
	mov	eax, [ecx+4*2]
	mov	[s12], eax
	mov	eax, [ecx+4*3]
	mov	[s20], eax
	mov	eax, [ecx+4*4]
	mov	[s21], eax
	mov	eax, [ecx+4*5]
	mov	[s22], eax
	ret
