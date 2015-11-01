(in-package :sb-vm)

(define-vop (%sse-add/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst addps sse-temp1 sse-temp2)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-addsub/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst addsubps sse-temp1 sse-temp2)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-div/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst divps sse-temp1 sse-temp2)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-hadd/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst haddps sse-temp1 sse-temp2)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-hsub/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst hsubps sse-temp1 sse-temp2)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-max/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst maxps sse-temp1 sse-temp2)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-min/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst minps sse-temp1 sse-temp2)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-mul/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst mulps sse-temp1 sse-temp2)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-sub/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst subps sse-temp1 sse-temp2)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-add/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst addpd sse-temp1 sse-temp2)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-addsub/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst addsubpd sse-temp1 sse-temp2)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-div/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst divpd sse-temp1 sse-temp2)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-hadd/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst haddpd sse-temp1 sse-temp2)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-hsub/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst hsubpd sse-temp1 sse-temp2)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-max/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst maxpd sse-temp1 sse-temp2)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-min/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst minpd sse-temp1 sse-temp2)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-mul/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst mulpd sse-temp1 sse-temp2)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-sub/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst subpd sse-temp1 sse-temp2)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-add/simple-array-unsigned-byte-8-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 0)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst paddb sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-avg/simple-array-unsigned-byte-8-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 0)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pavgb sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-max/simple-array-unsigned-byte-8-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 0)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pmaxub sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-min/simple-array-unsigned-byte-8-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 0)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pminub sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-sub/simple-array-unsigned-byte-8-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 0)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst psubb sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-and/simple-array-unsigned-byte-8-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 0)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pand sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-andn/simple-array-unsigned-byte-8-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 0)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pandn sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-or/simple-array-unsigned-byte-8-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 0)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst por sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-xor/simple-array-unsigned-byte-8-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-unsigned-byte-8
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 0)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pxor sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-add/simple-array-unsigned-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-16 simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst paddw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-avg/simple-array-unsigned-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-16 simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pavgw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-sub/simple-array-unsigned-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-16 simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst psubw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-and/simple-array-unsigned-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-16 simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pand sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-andn/simple-array-unsigned-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-16 simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pandn sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-or/simple-array-unsigned-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-16 simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst por sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-xor/simple-array-unsigned-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-16 simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pxor sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-shl/simple-array-unsigned-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-16 simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst psllw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-shr/simple-array-unsigned-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-16 simple-array-unsigned-byte-16
  simple-array-unsigned-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst psrlw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-add/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst paddw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-max/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pmaxsw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-min/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pminsw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-sub/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst psubw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-and/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pand sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-andn/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pandn sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-or/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst por sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-xor/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst pxor sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-shl/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst psllw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-shr/simple-array-signed-byte-16-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-signed-byte-16 simple-array-signed-byte-16
  simple-array-signed-byte-16 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 1)
  (inst movdqu sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst psraw sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop
 (%sse-andnot/simple-array-single-float/simple-array-unsigned-byte-8-1)
 (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-single-float
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst andnps sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-and/simple-array-single-float/simple-array-unsigned-byte-8-1)
 (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-single-float
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst andps sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-or/simple-array-single-float/simple-array-unsigned-byte-8-1)
 (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-single-float
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst orps sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-xor/simple-array-single-float/simple-array-unsigned-byte-8-1)
 (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-single-float
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst xorps sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop
 (%sse-andnot/simple-array-double-float/simple-array-unsigned-byte-8-1)
 (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-double-float
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst andnpd sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-and/simple-array-double-float/simple-array-unsigned-byte-8-1)
 (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-double-float
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst andpd sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-or/simple-array-double-float/simple-array-unsigned-byte-8-1)
 (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-double-float
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst orpd sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-xor/simple-array-double-float/simple-array-unsigned-byte-8-1)
 (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:arg-types simple-array-unsigned-byte-8 simple-array-double-float
  simple-array-unsigned-byte-8 fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movdqu sse-temp2
   (make-ea :dword :base vect2 :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst xorpd sse-temp1 sse-temp2)
  (inst movdqu
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-recip/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst rcpps sse-temp2 sse-temp1)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp2)))

(define-vop (%sse-rsqrt/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst rsqrtps sse-temp2 sse-temp1)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp2)))

(define-vop (%sse-sqrt/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (index :scs (unsigned-reg)))
 (:arg-types simple-array-single-float simple-array-single-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst sqrtps sse-temp2 sse-temp1)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp2)))

(define-vop (%sse-sqrt/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (index :scs (unsigned-reg)))
 (:arg-types simple-array-double-float simple-array-double-float fixnum)
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst sqrtpd sse-temp2 sse-temp1)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp2)))

(define-vop (%sse-cmp/simple-array-single-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:info cond)
 (:arg-types simple-array-single-float simple-array-single-float
  simple-array-single-float fixnum (:constant keyword))
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 2)
  (inst movups sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movups sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst cmpps sse-temp1 sse-temp2 cond)
  (inst movups
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

(define-vop (%sse-cmp/simple-array-double-float-1) (:policy :fast-safe)
 (:args (result :scs (descriptor-reg)) (vect1 :scs (descriptor-reg))
  (vect2 :scs (descriptor-reg)) (index :scs (unsigned-reg)))
 (:info cond)
 (:arg-types simple-array-double-float simple-array-double-float
  simple-array-double-float fixnum (:constant keyword))
 (:temporary (:sc xmm-reg) sse-temp1) (:temporary (:sc xmm-reg) sse-temp2)
 (:generator 10 (inst shl index 3)
  (inst movupd sse-temp1
   (make-ea :dword :base vect1 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst movupd sse-temp2
   (make-ea :dword :base vect2 :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)))
  (inst cmppd sse-temp1 sse-temp2 cond)
  (inst movupd
   (make-ea :dword :base result :index index :disp
    (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))
   sse-temp1)))

