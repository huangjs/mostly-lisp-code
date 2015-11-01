;:*=======================
;:* constructor
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;:*=======================
;:* selector
(define (left-branch m)
  (first-item m))

(define (right-branch m)
  (last-item m))

(define (branch-length b)
  (first-item b))

(define (branch-structure b)
  (last-item b))

;:*=======================
;:* application
;:* FIXME
(define (total-weight m)
