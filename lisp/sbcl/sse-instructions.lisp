(define-instruction addps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 88)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction addsubps (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 208) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction andnps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 85)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction andps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 84)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction divps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 94)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction haddps (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 124) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction hsubps (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 125) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction maxps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 95)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction minps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 93)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction mulps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 89)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction orps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 86)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction rcpps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 83)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction rsqrtps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 82)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction sqrtps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 81)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction subps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 92)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction unpckhps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 21)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction unpcklps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 20)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction xorps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 87)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction addpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 88) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction addsubpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 208) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction andnpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 85) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction andpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 84) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction divpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 94) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction haddpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 124) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction hsubpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 125) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction maxpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 95) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction minpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 93) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction mulpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 89) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction orpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 86) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction sqrtpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 81) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction subpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 92) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction unpckhpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 21) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction unpcklpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 20) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction xorpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 87) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction addsd (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 88) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction comisd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 47) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction divsd (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 94) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction maxsd (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 95) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction minsd (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 93) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction mulsd (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 89) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction sqrtsd (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 81) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction subsd (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 92) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction ucomisd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 46) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction addss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 88) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction comiss (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 47)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction divss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 94) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction maxss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 95) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction minss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 93) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction mulss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 89) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction rcpss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 83) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction rsqrtss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 82) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction sqrtss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 81) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction subss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 92) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction ucomiss (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 46)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction packssdw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 107) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction packsswb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 99) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction packuswb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 103) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction paddb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 252) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction paddd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 254) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction paddq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 212) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction paddsb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 236) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction paddsw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 237) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction paddusb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 220) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction paddusw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 221) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction paddw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 253) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pand (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 219) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pandn (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 223) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pavgb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 224) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pavgw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 227) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pcmpeqb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 116) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pcmpeqd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 118) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pcmpeqw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 117) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pcmpgtb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 100) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pcmpgtd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 102) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pcmpgtw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 101) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pmaddwd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 245) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pmaxsw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 238) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pmaxub (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 222) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pminsw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 234) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pminub (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 218) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pmovmskb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 215) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pmulhuw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 228) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pmulhw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 229) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pmullw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 213) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pmuludq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 244) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction por (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 235) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psadbw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 246) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pssld (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 242) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psllq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 243) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psllw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 241) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psrad (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 226) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psraw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 226) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psrld (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 210) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psrlq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 211) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psrlw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 209) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psubb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 248) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psubd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 250) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psubq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 251) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psubsb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 232) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psubsw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 233) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psubusb (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 216) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psubusw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 217) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction psubw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 249) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction punpckhbw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 104) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction punpckhdq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 106) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction punpckhqdq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 109) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction punpckhwd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 105) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction punpcklbw (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 96) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction punpckldq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 98) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction punpcklqdq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 108) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction punpcklwd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 97) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pxor (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 239) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtdq2pd (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 230) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtdq2ps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 91)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtpd2dq (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 230) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtpd2pi (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 45) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtpd2ps (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 90) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtpi2pd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 42) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtpi2ps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 42)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtps2dq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 91) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtps2pd (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 90)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtps2pi (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 45)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtsd2si (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 45) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtsd2ss (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 90) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtsi2sd (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 42) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtsi2ss (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 42) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtss2sd (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 90) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvtss2si (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 45) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvttpd2dq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 230) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvttpd2pi (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 44) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvttps2dq (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 91) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvttps2pi (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 44)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvttsd2si (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 44) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cvttss2si (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 44) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction lddqu (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 240) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction maskmovdqu (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 247) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movddup (segment dst src)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 18) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movhlps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 18)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movlhps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 22)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movmskpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 80) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movmskps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 80)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movntdq (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 231) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movntpd (segment dst src)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 43) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movntps (segment dst src)
 (:emitter (emit-byte segment 15) (emit-byte segment 43)
  (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movshdup (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 22) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction movsldup (segment dst src)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 18) (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction pextrw (segment dst src byte)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 197) (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte byte)))

(define-instruction pinsrw (segment dst src byte)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 196) (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte byte)))

(define-instruction pshufd (segment dst src byte)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 112) (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte byte)))

(define-instruction pshufhw (segment dst src byte)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 112) (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte byte)))

(define-instruction pshuflw (segment dst src byte)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 112) (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte byte)))

(define-instruction shufpd (segment dst src byte)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 198) (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte byte)))

(define-instruction shufps (segment dst src byte)
 (:emitter (emit-byte segment 15) (emit-byte segment 198)
  (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte byte)))

(define-instruction cmppd (segment dst src cond)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 194) (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte
   (cdr
    (assoc cond
           '((:eq . 0) (:e . 0) (:z . 0) (:l . 1) (:nge . 1) (:le . 2)
             (:ng . 2) (:unord . 3) (:ne . 4) (:nz . 4) (:nl . 5) (:ge . 5)
             (:nle . 6) (:g . 6) (:ord . 7)))))))

(define-instruction cmpps (segment dst src cond)
 (:emitter (emit-byte segment 15) (emit-byte segment 194)
  (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte
   (cdr
    (assoc cond
           '((:eq . 0) (:e . 0) (:z . 0) (:l . 1) (:nge . 1) (:le . 2)
             (:ng . 2) (:unord . 3) (:ne . 4) (:nz . 4) (:nl . 5) (:ge . 5)
             (:nle . 6) (:g . 6) (:ord . 7)))))))

(define-instruction cmpsd (segment dst src cond)
 (:emitter (emit-byte segment 242) (emit-byte segment 15)
  (emit-byte segment 194) (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte
   (cdr
    (assoc cond
           '((:eq . 0) (:e . 0) (:z . 0) (:l . 1) (:nge . 1) (:le . 2)
             (:ng . 2) (:unord . 3) (:ne . 4) (:nz . 4) (:nl . 5) (:ge . 5)
             (:nle . 6) (:g . 6) (:ord . 7)))))))

(define-instruction cmpss (segment dst src cond)
 (:emitter (emit-byte segment 243) (emit-byte segment 15)
  (emit-byte segment 194) (emit-ea segment src (reg-tn-encoding dst))
  (emit-sized-immediate segment :byte
   (cdr
    (assoc cond
           '((:eq . 0) (:e . 0) (:z . 0) (:l . 1) (:nge . 1) (:le . 2)
             (:ng . 2) (:unord . 3) (:ne . 4) (:nz . 4) (:nl . 5) (:ge . 5)
             (:nle . 6) (:g . 6) (:ord . 7)))))))

(define-instruction movapd (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 102) (emit-byte segment 15)
    (emit-byte segment 40) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 102) (emit-byte segment 15) (emit-byte segment 41)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movaps (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 15) (emit-byte segment 40)
    (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 15) (emit-byte segment 41)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movd (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 102) (emit-byte segment 15)
    (emit-byte segment 110) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 102) (emit-byte segment 15) (emit-byte segment 126)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movdqa (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 102) (emit-byte segment 15)
    (emit-byte segment 111) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 102) (emit-byte segment 15) (emit-byte segment 127)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movdqu (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 243) (emit-byte segment 15)
    (emit-byte segment 111) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 243) (emit-byte segment 15) (emit-byte segment 127)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movhpd (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 102) (emit-byte segment 15)
    (emit-byte segment 22) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 102) (emit-byte segment 15) (emit-byte segment 23)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movhps (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 15) (emit-byte segment 22)
    (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 15) (emit-byte segment 23)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movlpd (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 102) (emit-byte segment 15)
    (emit-byte segment 18) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 102) (emit-byte segment 15) (emit-byte segment 19)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movlps (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 15) (emit-byte segment 18)
    (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 15) (emit-byte segment 19)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movq (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 243) (emit-byte segment 15)
    (emit-byte segment 126) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 102) (emit-byte segment 15) (emit-byte segment 214)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movsd (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 242) (emit-byte segment 15)
    (emit-byte segment 16) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 242) (emit-byte segment 15) (emit-byte segment 17)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movss (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 243) (emit-byte segment 15)
    (emit-byte segment 16) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 243) (emit-byte segment 15) (emit-byte segment 17)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movupd (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 102) (emit-byte segment 15)
    (emit-byte segment 16) (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 102) (emit-byte segment 15) (emit-byte segment 17)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction movups (segment dst src)
 (:emitter
  (cond
   ((xmm-register-p dst) (emit-byte segment 15) (emit-byte segment 16)
    (emit-ea segment src (reg-tn-encoding dst)))
   (t (emit-byte segment 15) (emit-byte segment 17)
    (emit-ea segment dst (reg-tn-encoding src))))))

(define-instruction pslld-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 114) (emit-ea segment dst 6) (emit-byte segment amount)))

(define-instruction pslldq-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 115) (emit-ea segment dst 7) (emit-byte segment amount)))

(define-instruction psllq-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 115) (emit-ea segment dst 6) (emit-byte segment amount)))

(define-instruction psllw-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 113) (emit-ea segment dst 6) (emit-byte segment amount)))

(define-instruction psrad-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 114) (emit-ea segment dst 4) (emit-byte segment amount)))

(define-instruction psraw-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 113) (emit-ea segment dst 4) (emit-byte segment amount)))

(define-instruction psrld-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 114) (emit-ea segment dst 2) (emit-byte segment amount)))

(define-instruction psrldq-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 115) (emit-ea segment dst 3) (emit-byte segment amount)))

(define-instruction psrlq-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 115) (emit-ea segment dst 2) (emit-byte segment amount)))

(define-instruction psrlw-ib (segment dst amount)
 (:emitter (emit-byte segment 102) (emit-byte segment 15)
  (emit-byte segment 113) (emit-ea segment dst 2) (emit-byte segment amount)))

