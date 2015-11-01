;;; Gambit version of mat:
;; Compile: gsc -:s -cc-options "-O3 -U__SINGLE_HOST" mkmat.scm

(declare (standard-bindings)(extended-bindings)(block)(not safe))

(define iii 0)

(define-macro ($ v c i j)
  `(f64vector-ref ,v (fx+ (fx* ,i ,c) ,j) ) )

(define-macro ($! v c i j val)
  `(f64vector-set! ,v (fx+ (fx* ,i ,c) ,j) ,val))

(define-macro (!= x y)
  `(not (fx= ,x ,y)))

(define (prt m r c)
  (do ( (i 0 (fx+ i 1)) ) ( (fx>= i r) )
    (newline)
    (do ((j 0 (fx+ j 1)) )  ( (fx>= j c) )
      (display ($ m c i j) ) (display " ") (newline) )))

(define (make-system r)
  (let* ( (c (fx+ r 1))
          (m (make-f64vector (fx* r c) 0.0 ))
          (xx 0.0)
          (s 0.0) )

    (do ( (i 0 (fx+ i 1))  ) ( (fx>= i r) m)
      (set! s 0.0)
      (do ((j 0 (fx+ j 1) ) ) ( (fx>= j r) ($! m c i j  s) )
	(set! xx  (fl* 10.0 (random-real)))
	(set! s (fl+ s xx))
	($! m c i j xx )) )  ))

(define (swapit m c k l)
  (let ((t 0.0))
    (set! iii (fx+ iii 1))
    (do ( (j 0 (fx+ j 1)) ) ( (fx>= j c) )
      (set! t ($ m c k j ) )
      ($! m c k j  ($ m c l j) )
      ($! m c l j t) )  )  )

(define (find-max m c k i)
  (do ( (l (fx+ k 1) (fx+ l 1)) )
      ( (fx>= l (fx- c 1)) (if (!= i k)  (swapit m  c k i )))
    (if (fl> (flabs ($ m c l k)) (flabs ($ m c i k)) )
	(set! i l) )  ))

(define (solvit m r)
  (let ( (c (fx+ r 1))
	 (rat 0.0)
	 (mkk 0.0))

    (do ( (k 0 (fx+ k 1)) ) ( (fx>= k (fx- r 1)))
      (find-max m c k k)
      (set! mkk ($ m c  k k) )

      (do ( ( i (fx+ k 1)(fx+ i 1)) ) ( (fx>= i r))
	(set! rat (fl/  ($ m c i k) mkk  ))
	(do ( (j  k  (fx+ j 1))) ( (fx>= j c) )
	  ($! m c i j (fl- ($ m c i j)
			   (fl* rat ($ m c k j ) ) ) )
	  )
	)
      )

    (do ( (i (fx- r 1) (fx- i 1) ) ) ((fx< i 0) m)
      (do ( (j (fx+ i 1) (fx+ j 1))
	    (tx 0.0 (fl- tx (fl* ($ m c i j)
				 ($ m c j r    )) )) )
	  ( (fx>= j r)
	    ($! m c i r
		(fl/ (fl+ ($ m c  i r ) tx)
		     ($ m c i i)) ) ) ))
    )
  )

(define (main r)
  (let* ( (c (fx+ r 1)) (m (solvit (make-system r) r) ) )

    (do ( (i 0 (fx+ i 1))) ( (fx>= i (min r 10)) (newline) )
      (display  ($ m c i r) ) (display " ")  )
    (newline) (display "Number of swaps= ") (display iii) (newline)) )

;; (define-syntax incre
;;   (syntax-rules ()
;;     ((incre x) (begin (set! x (+ x 2)) x))))

;;; Gambit is not very bad for this kind of problem either. 
