(define-syntax curried
  (syntax-rules ()
    ((curried () body ...) (lambda () body ...))
    ((curried (arg) body ...) (lambda (arg) body ...))
    ((curried (arg args ...) body ...)
     (lambda (arg . rest)
       (let ((next (curried (args ...) body ...)))
         (if (null? rest)
             next
             (apply next rest)))))))

(define-syntax define-curried
  (syntax-rules ()
    ((define-curried (name args ...) body ...)
     (define name (curried (args ...) body ...)))))

;; example
;; (define-curried (mad a b c)
;;   (+ (* a b) c))

;; (mad 2 3 4)
;; ((mad 2) 3 4)
;; ((mad 2 3) 4)
;; (((mad 2) 3) 4)
