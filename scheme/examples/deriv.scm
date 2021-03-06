(define deriv-rules
  '( ( (dd (?c c) (? v))                 0)
    ( (dd (?v v) (? v))                 1)
    ( (dd (?v u) (? v))                 0)

    ( (dd (+ (? x1) (? x2)) (? v))
      (+ (dd (: x1) (: v))
         (dd (: x2) (: v)))              )

    ( (dd (* (? x1) (? x2)) (? v))
      (+ (* (: x1) (dd (: x2) (: v)))
         (* (dd (: x1) (: v)) (: x2)))   )

    ( (dd (** (? x) (?c n)) (? v))
      (* (* (: n)
            (** (: x) (: (- n 1))))
         (dd (: x) (: v)))               )
    ))

(define dsimp
  (simplifier deriv-rules))
 
(define (simplifier rules)
  (define (simplifier-exp exp)
    (try-rules (if (compound? exp)
                   (map simplify-exp exp)
                   exp)))
  (define (try-rules exp)
    (define (scan rules)
      ...)
    (scan rules))
  simplify-exp)

(define (match pat exp dict)
  (cond ((eq? dict 'failed 'failed)
         ((atom? pat)
          (if (atom? exp)
              (if (eq? pat exp)
                  dict
                  'failed)
              'failed))
         ((arbitrary-constant? pat)
          (if (constant? exp)
              (extend-dict pat exp dict)
              'failed))
         ((arbitrary-variable? pat)
          (if (variable? exp)
              (extend-dict pat exp dict)
              'failed))
         ((arbitrary-expression? pat)
          (extend-dict pat exp dict))
         ;;; Pattern variable clauses
         ((atom? exp) 'failed)
         (else
          (match (cdr pat)
                 (cdr exp)
                 (match (car pat)
                        (car exp)
                        dict))))))


(define (instantiate skeleton dict)
;:*=======================
;:* a recursive tree walk
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
             user-initial-environment)
       (mapcar (lambda (v)
                 (lookup v dict))
               (cdr form)))))

;:*=======================
;:* dictionary
(define (empty-dictionary) '())

(define (extend-dict pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((null? v)
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (null? v)
        var
        (cadr v))))
