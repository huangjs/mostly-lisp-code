;; shift/reset

(define *meta-cont* #f)

(define (abort t)
  (let ((v (t)))
    (*meta-cont* v)))

(define (shift h)
  (call/cc (lambda (k)
             (abort (lambda ()
                      (h (lambda (v)
						   (reset (lambda () (k v))))))))))

(define (reset t)
  (let ((old-meta-cont *meta-cont*))
    (call/cc (lambda (k)
               (set! *meta-cont* (lambda (v)
								   (set! *meta-cont* old-meta-cont)
								   (k v)))
               (abort t)))))

(+ 1 (reset (lambda () (* 2 (shift (lambda (k) (k (k 10))))))))

;; reflection and reification of monads

(define (reflect unit extend)
  (lambda (m)
    (shift (lambda (k) (extend k m)))))

(define (reify unit extend)
  (lambda (t)
    (extend unit (reset (lambda () (unit (t)))))))

;; exception monad

(define (ex-success x)
  (list 'success x))

(define (ex-error msg)
  (list 'error msg))

(define ex-unit ex-success)

(define (ex-extend f x)
  (case (car x)
    ((success) (f (cadr x)))
    ((error)   x)))

(define ex-reflect (reflect ex-unit ex-extend))

(define ex-reify (reify ex-unit ex-extend))

(define (ex-raise msg)
  (ex-reflect (ex-error msg)))

(define (ex-handle t h)
  (let ((rt (ex-reify t)))
    (case (car rt)
      ((success) (cadr rt))
      ((error)   (h (cadr rt))))))

(define (ex-run t)
  (ex-handle (lambda ()
               (string-append "OK: " (number->string (t))))
             (lambda (msg)
               (string-append "Error: " msg))))

(ex-run (lambda () (+ 1 2)))
(ex-run (lambda () (+ 1 (ex-raise "Oops!"))))

;; state monad

(define (st-unit x)
  (lambda (s)
    (list x s)))

(define (st-extend f m)
  (lambda (s)
    (let* ((v (m s))
           (x (car v))
           (s (cadr v)))
      ((f x) s))))

(define st-reflect (reflect st-unit st-extend))

(define st-reify (reify st-unit st-extend))

(define (st-fetch)
  (st-reflect (lambda (s) (list s s))))

(define (st-store x)
  (st-reflect (lambda (s) (list #f x))))

(define (st-tick)
  (st-reflect (lambda (s) (list #f (+ s 1)))))

(define (st-run s t)
  (car ((st-reify t) s)))

(st-run 0 (lambda ()
            (st-store 5)
            (st-tick)
            (* 2 (st-fetch))))

;; nondeterminism monad

(define nd-zero '())

(define nd-unit list)

(define nd-plus append)

(define (nd-extend f x)
  (if (eq? x nd-zero)
      nd-zero
      (nd-plus (f (car x))
               (nd-extend f (cdr x)))))

(define nd-reflect (reflect nd-unit nd-extend))

(define nd-reify (reify nd-unit nd-extend))

(define (nd-amb x y)
  (nd-reflect (nd-plus (nd-reify (lambda () x))
                       (nd-reify (lambda () y)))))

(define (nd-fail)
  (nd-reflect nd-zero))

(define (nd-run t)
  (nd-reify t))

(nd-run (lambda ()
          (let ((x (* (nd-amb 3 4) (nd-amb 5 7))))
            (if (>= x 20)
                x
                (nd-fail)))))

(nd-run (lambda ()
          (let ((x (nd-reflect (list 3 4 5)))
                (y (nd-reflect (list "foo" "bar"))))
			(list x y))))

;; continuation monad

(define (k-unit x)
  (lambda (k)
    (k x)))

(define (k-extend f t)
  (lambda (k)
    (t (lambda (v)
         ((f v) k)))))

(define k-reflect (reflect k-unit k-extend))

(define k-reify (reify k-unit k-extend))

(define (k-call/cc h)
  (k-reflect (lambda (c)
               (define (k v) (k-reflect (lambda _ (c v))))
               ((k-reify (lambda () (h k))) c))))

(define (k-shift h)
  (k-reflect (lambda (c)
               (define (k v) (k-reflect (lambda (c*) (c* (c v)))))
               ((k-reify (lambda () (h k)))
                (lambda (x) x)))))

(define (k-reset t)
  (k-reflect (lambda (c)
               (c ((k-reify t)
                   (lambda (x) x))))))

(define (k-run t)
  ((k-reify t) (lambda (x) x)))

(number->string (k-run (lambda ()
                         (+ 3 (k-call/cc (lambda (k) (+ 6 (k 1))))))))

(k-run (lambda ()
         (string-append "a" (k-reset (lambda ()
                                       (string-append "b" (k-shift (lambda (k)
                                                                     (k (k "c"))))))))))
