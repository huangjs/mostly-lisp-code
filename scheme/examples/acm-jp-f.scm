;;;; accessor and predicates
(define (left tree) (car tree))	
(define (right tree) (cadr tree))
(define (has-no-child? tree) (symbol? tree))

;;;; helper function
;;; similar to common lisp's cadr
(define (my-cadr x)
  (cond ((null? x) '())
        ((null? (cdr x)) '())
        (else (cadr x))))

;;; similar to that of common lisp
(define (remove-duplicates list)
  (let loop ((result '())
             (cursor list)
             (current (car list)))
    (if (null? cursor)
        (reverse result)
        (if (member current result)
            (loop result (cdr cursor) (my-cadr cursor))
            (loop (cons current result)
                  (cdr cursor)
                  (my-cadr cursor))))))

;;; union of two lists/sets
(define (union list1 list2)
  (remove-duplicates (append list1 list2)))

;;; intersection of two lists without duplications
(define (intersection list1 list2)
  (let ((list1 (remove-duplicates list1))
        (list2 (remove-duplicates list2)))
    (let loop ((result '())
               (cursor list1)
               (current (car list1)))
      (if (null? cursor)
          (reverse result)
          (if (member current list2)
              (loop (cons current result)
                    (cdr cursor)
                    (my-cadr cursor))
              (loop result (cdr cursor) (my-cadr cursor)))))))
    
;;; return all the children of a tree, a child is either a leaf or a
;;; tree
(define (all-children tree)
  ;; recursively append
  (if (has-no-child? tree)
      (list tree)
      (append (list tree)
      	      (all-children (left tree))
      	      (all-children (right tree)))))

;;; return the number of children of a tree
(define (count-children tree)
  ;; recursively add
  (if (has-no-child? tree)
      1
      (+ 1
         (count-children (left tree))
         (count-children (right tree)))))

;;; normalized structure has less children number on the left than on
;;; the right, and each of its children is a normalized structure
(define (normalize-structure tree)
  (if (has-no-child? tree)
      tree
      (if (> (count-children (left tree))
             (count-children (right tree)))
          (list (normalize-structure (right tree))
                (normalize-structure (left tree)))
          (list (normalize-structure (left tree))
                (normalize-structure (right tree))))))
      
;;; return all the structures of a tree, with duplications removed and
;;; structure normalized
(define (all-structures tree)
  (map normalize-structure
       (remove-duplicates (all-children tree))))


;;; left-right similarity of a tree
(define (lr-similarity tree)
  (if (has-no-child? tree)
      0
      (let ((left-structure (all-structures (left tree)))
            (right-structure (all-structures (right tree))))
        (/ (length (intersection left-structure right-structure))
           (length (union left-structure right-structure))))))

	   
;;; compare the asymmetricity of two trees, x and y; if x is stronger
;;; than y, return 'greater-than; if x is equal to y by its strength,
;;; return 'equal; otherwise, y is stronger than x, return 'less-than.
(define (compare-asymmetricity x y)
  (let ((lr-similarity-x (lr-similarity x))
        (lr-similarity-y (lr-similarity y)))
    (cond ((< lr-similarity-x lr-similarity-y)
           'greater-than)
          ((> lr-similarity-x lr-similarity-y)
           'less-than)
          ((and (has-no-child? x)
                (has-no-child? y))
           'equal)
          (else
           (let ((children-of-x (list-tree-by-stength (left x) (right x)))
                 (children-of-y (list-tree-by-stength (left y) (right y))))
             (let ((compare-first-children
                    (compare-asymmetricity (car children-of-x)
                                           (car children-of-y))))
               (cond ((eq? compare-first-children 'greater-than)
                      'greater-than)
                     ((eq? compare-first-children 'equal)
                      (compare-asymmetricity (cadr children-of-x)
                                             (cadr children-of-y)))
                     (else 'less-than))))))))

;;; make a list of x y ordered by their strength, stronger one is to
;;; the front, if they're equal, order is reserved.
(define (list-tree-by-stength x y)
  (if (eq? (compare-asymmetricity x y) 'less-than)
      (list y x)
      (list x y)))

;;; type tolerant reverse
(define (my-reverse x)
  (if (list? x)
      (reverse x)
      x))

;;; this is the answer of the problem.
(define (reorder-tree tree)
  (if (has-no-child? tree)
      tree
      (list-tree-by-stength (reorder-tree (left tree))
                            (reorder-tree (right tree)))))


;;; test of the example given by the problem, print #t or #f for each
;;; test
(define (test)
  (let ((inputs '((((x x) x) ((x x) (x (x x))))
                  (((x x) (x x)) ((x x) ((x x) (x x))))
                  (((x x) ((x x) x)) (((x (x x)) x) (x x)))
                  (((x x) x) ((x x) (((((x x) x) x) x) x)))
                  (((x x) x) ((x (x x)) (x (x x))))
                  ((((x (x x)) x) (x ((x x) x))) ((x (x x)) (x x)))
                  ((((x x) x) ((x x) (x (x x)))) (((x x) (x x)) ((x x) ((x x) (x x)))))))
        (outputs '(((x (x x)) ((x x) ((x x) x)))
                   (((x x) ((x x) (x x))) ((x x) (x x)))
                   (((x ((x x) x)) (x x)) ((x x) ((x x) x)))
                   (((x ((x ((x x) x)) x)) (x x)) ((x x) x))
                   ((x (x x)) ((x (x x)) ((x x) x)))
                   (((x (x x)) (x x)) ((x ((x x) x)) ((x (x x)) x)))
                   (((x (x x)) ((x x) ((x x) x))) (((x x) (x x)) (((x x) (x x)) (x x)))))))
    (let loop ((in inputs)
               (out outputs))
      (if (or (null? in) (null? out))
          (newline)
          (begin
            (display (equal? (reorder-tree (car in))
                            (reorder-tree (car out))))
            (loop (cdr in) (cdr out)))))))


;;; test on files, print #t or #f on each test.
(define (test-file input output)
  (let ((in-port (open-input-file input))
        (out-port (open-input-file output)))
    (let loop ((in (read in-port))
               (out (read out-port)))
      (if (or (null? in) (null? out) (eq? in 0) (eq? out 0))
          (newline)
          (begin
            (display (equal? (reorder-tree in)
                             (reorder-tree out)))
            (loop (read in-port) (read out-port)))))))


;;; program entry
(define (main)
  (test-file "F1" "F1.ans")
  (test-file "F2" "F2.ans")
  (test-file "F3" "F3.ans")
  (test-file "F4" "F4.ans"))

(main)
