(include "macros.scm")

(define var-pseudo-reg-table (make-parameter #f))

(define (var-pseudo-reg var #!optional (reg #f))
  (when (read-var? var)
    (set! var (read-var-var var)))
  (assert (var? var))
  (or (table-ref (var-pseudo-reg-table) var #f)
      (let* ((type (ast-node-type var))
             (name (var-name var))
             (pseudo-reg (or reg
                             (if (eq? type 'float)
                                 (intern-struct
                                  (make-pseudo-fp-reg (make-var-id) name))
                                 (intern-struct
                                  (make-pseudo-int-reg (make-var-id) name))))))
        (table-set! (var-pseudo-reg-table) var pseudo-reg)
        pseudo-reg)))

;; pattern:
;; children: node max-int max-fp -> #f... if fail or list of children, cost, nint, nfp, emit
;; emit: dst arg... -> code
;; nint, nfp: max live int/fp values (optimistic, let regalloc do its job) Return values count!

(define-macro (let*-if bindings . body)
  (let rec ((bindings bindings))
    (if (null? bindings)
        `(let ()
           ,@body)
        (let* ((binding (car bindings))
               (name    (car binding))
               (value   (cadr binding))
               (pred    (and (not (null? (cddr binding)))
                             (caddr binding))))
          `(let ((,name ,value))
             (if ,(cond ((not pred)
                         #t)
                        ((eq? pred #t)
                         name)
                        ((symbol? pred)
                         `(,pred ,name))
                        (#t 
                         pred))
                 ,(rec (cdr bindings))
                 ;; children cost nint nfp emit
                 (values #f #f #f #f #f)))))))

(define default-pattern-list '())

(define-macro (define-pattern name bindings #!key
                (prologue '()) (body '()) (dst 'dst)
                children children-form (cost 0) (nint 0) (nfp 0) emit emit-form)
  (if (not (and (not (and children children-form))
                (or emit emit-form)
                (not (and emit emit-form))
                (or (not children-form) emit-form)))
      (error "Bad pattern"))

  (set! children (or children '()))

  (let ((name (string->symbol (string-append (symbol->string name) "-pattern"))))
    `(begin
       (define (,name node max-int max-fp)
         ,@prologue
         (let*-if ,bindings
           ,@body
           (values ,(if children-form
                        children-form
                        `(list ,@children))
                   ,cost ,nint ,nfp
                   ,(if emit-form
                        emit-form
                        `(lambda (,@(if dst `(,dst) '())
                                  ,@children)
                           ,emit)))))

       (if (not (memq ,name default-pattern-list))
           (set! default-pattern-list (cons ,name default-pattern-list)))

       (void))))

(include "codegen-patterns.scm")

(define-type code-snippet
  cost
  nint nfp
  nint-res nfp-res
  emitter   ;; dst-reg arg-regs... -> insn list designator
  children  ;; list of snippets
  expr)

(define impossible-snippet (make-code-snippet
                            +inf.0
                            0 0
                            0 0 
                            #f '() #f))

(define pattern-list  (make-parameter #f))

(define total-int-reg     (make-parameter 15))
(define total-fp-reg      (make-parameter 16))
(define emit-expr-table   (make-parameter #f))
(define spill-cost        (make-parameter 1))
(define snippet-reg-table (make-parameter #f))

(define (create-register name expr)
  (let ((type (and (expression-node? expr)
                   (expression-node-type expr))))
    (and type
         (intern-struct
          (cond ((member type '(int (array int) (array float)))
                 (make-pseudo-int-reg  (make-var-id) name))
                ((eq? type 'float)
                 (make-pseudo-fp-reg   (make-var-id) name))
                (#t (error `("Unknown expression type" ,type))))))))

(define (snippet-spill-reg snippet)
  (let ((table (snippet-reg-table)))
    (or (table-ref table snippet #f)
        (let ((register (create-register 'spill (code-snippet-expr snippet))))
          (table-set! table snippet register)
          register))))

(define (snippet-children-set snippet)
  (let ((snippets (make-struct-table))
        (max-children -1))
    (let rec ((snippet snippet))
      (unless (table-ref snippets snippet #f)
        (table-set! snippets snippet #t)
        (set! max-children
              (max max-children
                   (length (code-snippet-children snippet))))
        (for-each rec (code-snippet-children snippet))))

    (values snippets max-children)))

(define (flatten-snippet snippet)
  (let ((flattened-snippets '())
        (emitted-snippets (make-struct-table)))

    (define (snippet-wait-count snippet)
      (count (lambda (child)
               (not (table-ref emitted-snippets child #f)))
             (code-snippet-children snippet)))

    (receive (snippets max-count)
             (snippet-children-set snippet)

      (define (emit-snippet snippet)
        (set! flattened-snippets (cons snippet flattened-snippets))
        (table-set! emitted-snippets snippet #t)
        (table-set! snippets snippet))

      (define (find-best-snippet)
        (let ((max-size  #f)
              (best      #f))
          (table-for-each (lambda (snippet v) 
                            v
                            (let ((size (max (code-snippet-nint snippet)
                                             (code-snippet-nfp  snippet))))
                              (when (and (= 0 (snippet-wait-count snippet))
                                         (or (not max-size)
                                             (< max-size size)))
                                    (set! max-size size)
                                    (set! best snippet))))
                          snippets)
          best))

      (do ()
          ((= 0 (table-length snippets))
           flattened-snippets)
        (let ((best (find-best-snippet)))
          (assert best)
          (emit-snippet best))))))

(define (emit-expr expr #!optional patterns total-int total-fp)
  (parameterize ((pattern-list  (or patterns
                                    default-pattern-list))
                 (total-int-reg (or total-int
                                    (total-int-reg)))
                 (total-fp-reg  (or total-fp
                                    (total-fp-reg)))
                 (emit-expr-table (make-struct-table))
                 (snippet-reg-table (make-struct-table)))
    (let ((root-snippet (%emit-expr (if (assign-var? expr)
                                        (assign-var-value expr)
                                        expr)
                                    (total-int-reg) (total-fp-reg))))
      (assert (not (= +inf.0 (code-snippet-cost root-snippet))))
      (append-map (lambda (snippet)
                    ((code-snippet-emitter snippet)
                     (if (and (eq? root-snippet snippet)
                              (assign-var? expr))
                         (var-pseudo-reg (assign-var-var expr))
                         (snippet-spill-reg snippet))))
                  (flatten-snippet root-snippet)))))

(define (%emit-expr expr max-int max-fp)
  (let ((table (emit-expr-table)))
    (cond ((table-ref table 
                      (list expr max-int max-fp) 
                      #f))
          (#t
           (let rec ((patterns (pattern-list)) (best impossible-snippet))
             (if (null? patterns)
                 (do ((nint (code-snippet-nint best) (+ nint 1)))
                     ((> nint max-int)
                      best)
                   (do ((nfp (code-snippet-nfp best) (+ nfp 1)))
                       ((> nfp max-fp))
                     (table-set! table (list expr nint nfp) best)))

                 (let ((new (try-pattern (car patterns) expr max-int max-fp)))
                   (rec (cdr patterns)
                        (if (< (code-snippet-cost new)
                               (code-snippet-cost best))
                            new
                            best)))))))))

(define (try-pattern pattern expr max-int max-fp)
  (receive (children cost nint nfp emit-fun)
           (pattern expr max-int max-fp)
    (cond ((not (and children cost nfp nint emit-fun))
           impossible-snippet)
          (#t
           (assert (every expression-node? children)) ;; can't have statements as children!
           (receive (children-cost children-snippets max-live-int max-live-fp emitter)
                    (emit-children children max-int max-fp)
             (let ((type (and (expression-node? expr)
                              (expression-node-type expr))))
               (assert (member type '(int float #f (array int) (array float))))

               (make-code-snippet
                (+ cost children-cost)
                (max nint max-live-int)
                (max nfp max-live-fp)
                (if (member type '(int (array int) (array float)))
                    1 0)
                (if (eq? type 'float) 1 0)
                (lambda (dst)
                  (receive (args code)
                           (emitter)
                           (append code
                                   (listify
                                    (if dst
                                        (apply emit-fun dst args)
                                        (apply emit-fun args))))))
                children-snippets
                expr)))))))

(define (compute-emit-order children max-int max-fp)
  (let ((total-int  (total-int-reg))
        (total-fp   (total-fp-reg))
        (spill-cost (spill-cost)))
    (let rec ((todo children) (cost +inf.0) (incumbent #f) (live-int 0) (live-fp 0))
      (cond ((null? todo)
             (cond ((null? children)
                    (values 0 '() 0 0))
                   (#t
                    (assert incumbent)
                    (values cost incumbent live-int live-fp))))
            (#t
             (let* ((child (car children))
                    (inline-snippet (%emit-expr child max-int max-fp))
                    (spill-snippet  (%emit-expr child total-int total-fp))
                    (spill?         (< (+ spill-cost (code-snippet-cost spill-snippet))
                                       (code-snippet-cost inline-snippet)))
                    (snippet (if spill? spill-snippet inline-snippet)))
               (assert (not (= +inf.0 (code-snippet-cost spill-snippet))))

               (receive (rest-cost rest-plan max-int max-fp)
                        (compute-emit-order
                         (delete child children)
                         (- max-int
                            (code-snippet-nint-res snippet))
                         (- max-fp
                            (code-snippet-nfp-res snippet)))
                 (let ((total-cost (+ (code-snippet-cost snippet) rest-cost)))
                   (if (< total-cost cost)
                       (rec (cdr todo)
                            total-cost
                            (cons (cons snippet spill?)
                                  rest-plan)
                            (max (+ (code-snippet-nint-res snippet)
                                    max-int)
                                 (if spill? -1
                                     (code-snippet-nint snippet)))
                            (max (+ (code-snippet-nfp-res snippet)
                                    max-fp)
                                 (if spill? -1
                                     (code-snippet-nfp snippet))))
                       (rec (cdr todo)
                            cost incumbent
                            live-int live-fp))))))))))

(define (emit-children children max-int max-fp)
  (receive (cost plan max-int max-fp) (compute-emit-order children max-int max-fp)
     ;; cost spilled-children max-int max-fp emitter
     (values cost 
             (map car (remove (lambda (x)
                                (not (cdr x)))
                              plan))
             max-int max-fp
             (lambda ()
               (let* ((code '())
                      (get-child-reg
                       (lambda (child)
                         (cond ((cdr child)
                                (snippet-spill-reg (car child)))
                               ((read-var? (code-snippet-expr
                                            (car child)))
                                (var-pseudo-reg (code-snippet-expr
                                                 (car child))))
                               (#t
                                (let ((dst (create-register 'tmp (code-snippet-expr
                                                                  (car child))))
                                      (emitter (code-snippet-emitter
                                                (car child))))
                                  (set! code
                                        (append code (emitter dst)))
                                  dst)))))
                      (args (map-in-order
                             (lambda (child)
                               (cons (code-snippet-expr (car child))
                                     (get-child-reg child)))
                             plan)))
                 
                 (values (map (lambda (child)
                                (cond ((assoc child args) => cdr)
                                      (#t (error "Child not in children expression list?!"))))
                              children)
                         code))))))

(define (emit-statements stmt)
  (assert (seq-section? stmt))
  (assert (not (any (lambda (stmt)
                      (or (seq-section? stmt)
                          (par-section? stmt)))
                    (seq-section-statements stmt))))
  (parameterize ((var-pseudo-reg-table (make-struct-table)))
    (append-map emit-expr
                (seq-section-statements stmt))))
