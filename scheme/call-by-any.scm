; Parameterized syntax: interpreters without run-time interpretive overhead
;
; Our source language is a subset of Scheme:
;  (e e1 ...)            -- application
;  (lambda (x ...) e)    -- abstraction
;  (cmd any-scheme-expr) -- any scheme expression. It must not contain any 
;                           free variables (except standard Scheme bindings)
;                           This is the `FFI' call to the host Scheme
;                           system.
;  v		         -- numbers, strings and other Scheme literal constants
;  (quote v)             -- quoted constants and values
;  (begin e ...)         -- sequencing
;  (* e1 e2)             -- primitives
;  (sub1 e1)
;  (zero? e1)
;  var
;
; We show how to interpret a source language expression in several ways,
; without dispatch on syntax at run-time. Rather, we dispatch on syntax
; at the macro-expand time. The R5RS macro xlate below takes an
; expression and the environment. The environment is an associative list
; that determines the meaning of the application, sequencing, lambda,
; injection of values, and the interpretation of primitives.
; The macro xlate traverses the given source language expression,
; detecting primitive forms and replacing them using the environment.
; The resulting Scheme expression (with re-defined built-ins and
; special forms) can be evaluated or compiled by the host Scheme
; system as usual. To avoid the overhead of searching variable
; environment, we use higher-order abstract syntax.

; Our main example is evaluating a source language expression using
; call-by-value, call-by-ref, and call-by-name evaluation
; strategies.


; (mlookup str assoc-list) 
; Lookup up an item in an assoc list, at macro-expand time. The key is
; a string.
; (mlookup "x" (("y" 1) ("x" 2) ("zz" 3)) 5 6) expands to (2 5 6).
(define-syntax mlookup
  (syntax-rules ()
    ((_ key body . k0)
      (letrec-syntax
	((do-lkup
	  (syntax-rules ()
	    ((_ k ((key val) . rest)) (val . k))
	    ((_ k (x . rest)) (do-lkup k rest)))))
	(do-lkup k0 body)))))

;(expand '(mlookup "x" (("y" 1) ("x" 2) ("zz" 3)) 5 6))
;,expand (mlookup "x" (("y" 1) ("x" 2) ("zz" 3)) 5 6)
; (2 5 6)
;(expand '(mlookup "zz" (("y" 1) ("x" 2) ("zz" if)) 4 5 6))
; (if 4 5 6)
(mlookup "zz" (("y" 1) ("x" +) ("zz" if)) 4 5 6)
; 5
(mlookup "x"  (("y" 1) ("x" +) ("zz" if)) 4 5 6)
; 15

; (mlookup "zzz" (("y" 1) ("x" 2) ("zz" 3)))
; 'syntax-error


; A symbol? predicate at the macro-expand time
;	symbol?? FORM KT KF
; FORM is an arbitrary form or datum
; expands in KT if FORM is a symbol (identifier), Otherwise, expands in KF

(define-syntax symbol??
  (syntax-rules ()
    ((symbol?? (x . y) kt kf) kf)	; It's a pair, not a symbol
    ((symbol?? #(x ...) kt kf) kf)	; It's a vector, not a symbol
    ((symbol?? maybe-symbol kt kf)
      (let-syntax
	((test
	   (syntax-rules ()
	     ((test maybe-symbol t f) t)
	     ((test x t f) f))))
	(test abracadabra kt kf)))))


; We translate an expression with respect to the "env" that defines
; the domain and provides the meaning for primitives.
(define-syntax xlate
  (syntax-rules (* sub1 zero? lambda quote cmd begin)
    ((_ env (* e1 e2))  (mlookup "*" env (xlate env e1) (xlate env e2)))
    ((_ env (sub1 e1))  (mlookup "sub1" env (xlate env e1)))
    ((_ env (zero? e1)) (mlookup "zero?" env (xlate env e1)))
    ((_ env (lambda vars body))
      (mlookup "lambda" env (lambda vars (xlate env body)))) ; HOAS
    ((_ env (quote v)) (mlookup "inj" env 'v))
    ((_ env (cmd e)) (mlookup "app" env	                     ; FFI `call'
		       (mlookup "inj" env (lambda () e 
					    (mlookup "inj" env '())
					    ))))
    ((_ env (begin e)) (xlate env e))
    ((_ env (begin e1 e2 ...))
      (mlookup "seq" env (xlate env e1) (xlate env (begin e2 ...))))
    ((_ env (e ...)) (mlookup "app" env (xlate env e) ...))
    ((_ env x) 
      (symbol?? x
	x			; variables represent themselves
	(mlookup "inj" env x))  ; other constants
      )))

; simple tests and examples

;; ,expand (xlate (("*" star) ("inj" in) ("lambda" in)) 
;; 	  (lambda (x) (lambda (y) (* x y))))
;;=> '(in (lambda (x) (in (lambda (y) (star x y)))))

;; ,expand (xlate (("*" star) ("inj" in) ("lambda" in)) 
;; 	   (lambda (x) (* 3 x)))
;; '(in (lambda (x) (star (in 3) x)))

;; ,expand (xlate (("*" star) ("inj" in) ("lambda" in) ("app" app))
;;  	   (lambda (x) (lambda (f) (f x '()))))
;; '(in (lambda (x) (in (lambda (f) (app f x (in '()))))))

; We interpret * as +
(let ((star +) (in (lambda (x) x)))
  (let ((f (xlate (("*" star) ("inj" in) ("lambda" in)) 
	     (lambda (x) (lambda (y) (* x y))))))
    ((f 1) 2)))
; 3

; Example 1
; The standard interpretation: interpret a Scheme expression as the host
; Scheme system would interpret it
(define-syntax standard
  (syntax-rules ()
    ((standard exp)
      (let-syntax 
	((id  (syntax-rules () ((id x) x)))
	 (app (syntax-rules () ((app x . xs) (x . xs)))))
	(xlate
	  (("inj" id) ("*" *) ("sub1" (lambda (x) (- x 1))) ("zero?" zero?)
	   ("lambda" id) ("app" app) ("seq" begin))
	  exp)))))

(standard ((lambda (x) (* (x 2) 3)) (lambda (n) (* n n))))
; 12

; Example 2
; Interpreting an expression to count the number of constructors
; An expression is evaluated to a number, the number of `constructors'
; in the expression.

(define-syntax count-ctr
  (syntax-rules ()
    ((count-ctr exp)
      (let ((ctr1 (lambda (x) 1))
	    (op   (lambda args (apply + 1 args))))
	(xlate
	  (("inj" ctr1) ("*" op) ("sub1" op) ("zero" op)
	   ("lambda" (lambda (f) (+ 1 (f 0)))) ; a variable counts as 0
	   ("app" op) ("seq" op))
	  exp)))))

; Tests of counting
(count-ctr (* 1 2))
; 3   that is, two constants and +

(count-ctr (lambda (x) (* x 2)))
; 3    a variable counts as 0, lambda counts as 1

(count-ctr (lambda (x) (x 2)))
; 3    app counts as 1

(count-ctr ((lambda (x) (* (x 2) 3)) (lambda (n) (* n n))))
; 8

(count-ctr (lambda (x) (lambda (f) (f x '()))))
; 4  two lambdas, one app and one constant


; The main example: evaluating an expression using call-by-ref
; and call-by-name semantics (see `standard' for the call-by-value
; semantics)

; call-by-ref

(define box vector)
(define (unbox x) (vector-ref x 0))
(define (set-box! x v) (vector-set! x 0 v))

(define-syntax call-by-ref
  (syntax-rules ()
    ((_ exp)
      (let ((star  (lambda (x y) (box (* (unbox x) (unbox y)))))
	    (sub1  (lambda (x) (box (- (unbox x) 1))))
	    (zero? (lambda (x) (box (zero? (unbox x)))))
	    (inj   box)
	    (app   (lambda (op . args) (apply (unbox op) args)))
	    (seq   (lambda (e1 e2) (begin e1 e2))))
	(xlate (("*" star) ("sub1" sub1) ("zero?" zero?)
		("inj" inj) ("app" app) ("seq" seq) ("lambda" inj))
	  exp)))))

;; ,expand (call-by-ref ((lambda (x) (* x x)) 3))

(call-by-ref ((lambda (x) (* x x)) 3))
; '#(9)   that is, boxed 9

; cmd should not contain any free var. But we know what we are doing...
;; (expand '(call-by-ref 
;; 	   ((lambda (x) (* (begin (cmd (set-box! x 10)) x) x)) (inj 3))))
(call-by-ref 
	   ((lambda (x) (* (begin (cmd (set-box! x 10)) x) x)) 3))
; '#(100)


; call-by-name

(define-syntax freeze 
  (syntax-rules ()
    ((_ e) (lambda () e))))
(define (thaw v) (v))

(define-syntax call-by-name
  (syntax-rules ()
    ((_ exp)
      (let-syntax ((seq (syntax-rules ()
			  ((_ e1 e2) (freeze (begin (thaw e1) (thaw e2)))))))
       (let ((star  (lambda (x y) (freeze (* (thaw x) (thaw y)))))
	     (sub1  (lambda (x)   (freeze (- (thaw x) 1))))
	     (zero? (lambda (x)   (freeze (zero? (thaw x)))))
	     (app   (lambda (op . args)
		      (freeze (thaw (apply (thaw op) args))))))
	(xlate (("*" star) ("sub1" sub1) ("zero?" zero?)
		("inj" freeze) ("app" app) ("seq" seq) ("lambda" freeze))
	  exp))))))

;; (expand '(call-by-name
;;    ((lambda (x) (* x x)) (inj 3))))

(thaw (call-by-name
   ((lambda (x) (* x x)) 3)))
; 9

(call-by-name
   ((lambda (x) (* x x)) (begin (cmd (begin (write '&) (newline))) 3)))
; no effect is made: no demand

(thaw (call-by-name
   ((lambda (x) (* x x)) (begin (cmd (begin (write '&) (newline))) 3))))
; result 9  and twice the printing

; call-by-need is the combination of call-by-name and call-by-ref.
; It is left as an exercise for the reader...
