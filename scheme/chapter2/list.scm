;:*=======================
;:* List operations
;:* primitive: (list <a1> <a2> <a3> ...)


;:*=======================
;:* items[n]
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;:*=======================
;:* length of list
;:* recursive
(define (length items)
  (if (null? items)
      0
      (+ 1
         (length (cdr items)))))

;:*=======================
;:* iterative
(define (length items)
  (define (iter items count)
    (if (null? items)
        count
        (iter (cdr items) (+ count 1))))
  (iter items 0))

;:*=======================
;:* append
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;:*=======================
;:* last-pair
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;:*=======================
;:* first-item
(define first-item car)

;:*=======================
;:* last-item
(define (last-item items)
  (if (null? (cdr items))
      (car items)
      (last-item (cdr items))))

;:*=======================
;:* reverse
;:* iterative
(define (reverse items)
  (define (iter items result)
    (if (null? items)
        result
        (iter (cdr items)
              (cons (car items) result))))
  (iter items '()))

;:*=======================
;:* filter items with rules
(define (filter predicate items)
  (if (null? items)
      '()
      (if (predicate (car items))
          (cons (car items) (filter predicate (cdr items)))
          (filter predicate (cdr items)))))

;:*=======================
;:* map items to list of f(i)
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

;:*=======================
;:* for-each
;:* similar to map
(define (for-each items proc)
  (if (null? items)
      ()
      (begin
        (proc (car items))
        (for-each (cdr items) proc))))

;:*=======================
;:* application
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (y) (cons (car s) y)) rest)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
