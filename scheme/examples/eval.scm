;:*=======================
;:* car and cdr should be named.
;:* this is just an demonstration
;;; NOTE: the style is UGLY.
(define eval
  (lambda (exp env)
    (cond
     ;;; special form
     ((number? exp) exp) ;;; 3 -> 3
     ((symbol? exp) (lookup exp env)) ;;; x -> 3
     ((eq? (car exp) 'quote)
      (cadr exp)) ;;; 'foo => (quote foo) -> foo
     ((eq? (car exp) 'lambda)
      (list 'closure (cdr exp) env)) ;;; lambda (x) (body) => (closure ((x) (body)) env)
     ((eq? (car exp) 'cond)
      (evalcond (cdr exp) env)) ;;; (cond ((p1 e1)) ((p2 e2))) 
     ;;; end of special form
     (else
      (apply (eval (car exp) env)
             (evallist (cdr exp) env))))))


(define apply
  (lambda (proc args)
    (cond ((primitive? proc)
           (apply-primop proc args))
          ((eq? (car proc) 'closure)
           (eval (cadadr proc)
                 (bind (caadr proc)
                       args
                       (caddr proc))))
          (else
           (error "some sort of error.")))))


(define evallist
  (lambda (l env)
    (cond ((eq? l nil) nil)
          (else
           (cons (eval (car l) env)
                 (evallist (cdr l) env))))))

(define evalcond
  (lambda (clauses env)
    (cond ((eq? clauses nil) nil)
          ((eq? (caar clauses) 'else)
           (eval (cadar clauses) env))
          ((false? (eval (caar clauses) env))
           (evalcond (cdr clauses) env))
          (else ;;; when hit true clause
           (eval (cadar clauses) env)))))

(define bind
  (lambda (vars vals env)
    (cond ((pair-up vars vals)
           env))))

(define pair-up
  (lambda (vars vals)
    (cond ((eq? vars nil)
           (cond ((eq? vals nil) nil)
                 (else (error "Too many arguments."))))
          ((eq? vals '()) (error "Too few arguments."))
          (else
           (cons (cons (car vars)
                       (car vals))
                 (pair-up (cdr vars)
                          (cdr vals)))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env nil)
           (error "Unbounded variable."))
          (else
           ((lambda (vcell)
              (cond ((eq? vcell nil)
                     (lookup sym
                             (cdr env)))
                    (else cdr vcell)))
            (assq sym (car env)))))))
                 
(define assq
  (lambda (sym alist)
    (cond ((eq? alist nil) nil)
          ((eq? sym (caar alist))
           (car alist))
          (else
           (assq sym (cdr alist))))))

