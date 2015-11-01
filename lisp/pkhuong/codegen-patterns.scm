;;; Included from codegen.scm

;; this is actually special-cased by the codegen
(define-pattern var
  ((node node read-var?)
   (type (expression-node-type node))
   (reg  (var-pseudo-reg node)))
  cost: 1
  emit: (if (eq? type 'int)
            (make-move-int dst reg)
            (make-move-fp  dst reg)))

;; and so is that...
(define-pattern assign-var
  ((node node assign-var?)
   (var  (assign-var-var node))
   (reg  (var-pseudo-reg var))
   (val  (assign-var-value node))
   (int? (eq? 'int (expression-node-type val))))
  dst: #f
  children: (val)
  cost: 1
  nint: (if int? 1 0)
  nfp:  (if int? 0 1)
  emit: (if int?
            (make-move-int reg val)
            (make-move-fp reg val)))

(define-pattern constant
  ((node  node read-constant?)
   (value (read-constant-value node)))
  cost: 1
  emit: (if (integer? value)
            (make-load-constant-int dst value)
            (make-load-constant-fp dst value)))

(define-pattern read-array
  ((node  node read-array?)
   (int?  (eq? 'int (expression-node-type node)))
   (array (read-array-array node))
   (index (read-array-index node)))
  children: (array index)
  cost: 1
  nint: 2
  nfp:  (if int? 0 1)
  emit: ((if int? make-move-int make-move-fp)
         dst
         (make-ea base: array index: index stride: 8)))

(define-pattern read-array-constant-index
  ((node  node read-array?)
   (int?  (eq? 'int (expression-node-type node)))
   (array (read-array-array node))
   (index (read-array-index node) (read-constant? index))
   (index (read-constant-value index)))
  children: (array)
  cost: 1
  nint: 2
  nfp:  (if int? 0 1)
  emit: ((if int? make-move-int make-move-fp)
         dst
         (make-ea base: array offset: (* 8 index))))

(define-pattern assign-array
  ((node  node assign-array?)
   (array (assign-array-array node))
   (index (assign-array-index node))
   (value (assign-array-value node))
   (int?  (eq? 'int (expression-node-type value))))
  dst: #f
  children: (array index value)
  cost: 3
  nint: (if int? 3 2)
  nfp:  (if int? 0 1)
  emit: ((if int? make-move-int make-move-fp)
         (make-ea base: array index: index stride: 8)
         value))

(define-pattern assign-array-constant-index
  ((node  node assign-array?)
   (array (assign-array-array node))
   (index (assign-array-index node) read-constant?)
   (index (read-constant-value index))
   (value (assign-array-value node))
   (int?  (eq? 'int (expression-node-type value))))
  dst: #f
  children: (array value)
  cost: 3
  nint: (if int? 3 2)
  nfp:  (if int? 0 1)
  emit: ((if int? make-move-int make-move-fp)
         (make-ea base: array offset: (* index 8))
         value))

(define-pattern add
  ((node node +-expression?)
   (int? (eq? 'int (expression-node-type node)))
   (x    (binary-expression-x node))
   (y    (binary-expression-y node)))
  children: (x y)
  cost: (if int? 1 2)
  nint: (if int? 2 0)
  nfp:  (if int? 0 2)
  emit-form: (if int? make-add-int make-add-fp))

(define-pattern add-constant
  ((node node +-expression?)
   (type (expression-node-type node) (eq? 'int type))
   (x    (binary-expression-x node) read-constant?)
   (value (read-constant-value x))
   (y    (binary-expression-y node)))
  children: (y)
  cost: 1
  nint: 1
  emit: (make-add-int dst value y))

(define-pattern add-constant-mul-constant
  ((node node +-expression?)
   (type (expression-node-type node) (eq? 'int type))
   (x    (binary-expression-x node) read-constant?)
   (value (read-constant-value x))
   (mul  (binary-expression-y node) *-expression?)
   (multiplier (binary-expression-x mul) read-constant?)
   (multiplier (read-constant-value multiplier) (memq multiplier '(1 2 4 8)))
   (y    (binary-expression-y mul)))
  children: (y)
  cost: 2
  nint: 1
  emit: (make-lea dst (make-ea index: y stride: multiplier offset: value)))

(define-pattern mul
  ((node node *-expression?)
   (int? (eq? 'int (expression-node-type node)))
   (x    (binary-expression-x node))
   (y    (binary-expression-y node)))
  children: (x y)
  cost: (if int? 3 6)
  nint: (if int? 2 0)
  nfp:  (if int? 0 2)
  emit-form: (if int? make-mul-int make-mul-fp))

(define-pattern mul-constant-int
  ((node node *-expression?)
   (type (expression-node-type node) (eq? 'int type))
   (x    (binary-expression-x node) read-constant?)
   (value (read-constant-value x))
   (y    (binary-expression-y node)))
  children: (y)
  cost: 3
  nint: 2
  emit: (cond ((= value 2) ;; see AMD optimisation guide for all values
               (list (make-move-int dst y)
                     (make-add-int dst dst dst)))
              ((memq value '(1 2 4 8))
               (make-lea dst (make-ea index: y stride: value)))
              (#t
               (make-mul-int dst value y))))
