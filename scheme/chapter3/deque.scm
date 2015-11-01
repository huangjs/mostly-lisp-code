;:*=======================
;:* content < item < deque

(define (make-item x predecessor successor)
  (cons x (cons predecessor successor)))

(define (empty-item? item)
  (null? item))

(define (content item)
  (car item))

(define (predecessor item)
  (cadr item))

(define (successor item)
  (cddr item))

(define (set-predecessor! item predecessor-item)
  (set-car! (cdr item) predecessor-item))

(define (set-successor! item successor-item)
  (set-cdr! (cdr item) successor-item))

;:*-------------------------------- abstract layer

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

;:*----------------------------------- building bricks

(define (make-deque)
  (cons '() '()))

(define (empty-deque? deque)
  (null? (front-ptr deque)))

;:*=======================
;:* return item
(define (front-of-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (front-ptr deque)))

;:*=======================
;:* return item
(define (rear-of-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (rear-ptr deque)))

;:*=======================
;:* return item
(define (second-of-deque deque)
  (if (empty-deque? deque)
      (error "SECOND-OF-DEQUE called with an empty deque" deque)
      (successor (front-ptr deque))))

;:*=======================
;:* return deque
(define (rest-of-deque deque)
  (define q (make-deque))
  (cond ((empty-deque? deque)
         (error "the queue is empty" deque))
        ((empty-deque? (second-of-deque deque))
         ())
        (else
         (set-front-ptr! q (successor (front-ptr deque)))
         (set-rear-ptr! q (rear-ptr deque))))
  q)

(define (front-insert-deque! deque x)
  (let ((new-item (make-item x '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item))
          (else
           (set-successor! new-item (front-of-deque deque))
           (set-predecessor! (front-of-deque deque) new-item)
           (set-front-ptr! deque new-item)))
    deque))

(define (rear-insert-deque! deque x)
  (let ((new-item (make-item x '() '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-item)
           (set-rear-ptr! deque new-item))
          (else
           (set-predecessor! new-item (rear-of-deque deque))
           (set-successor! (rear-of-deque deque) new-item)
           (set-rear-ptr! deque new-item)))
    deque))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE called with an empty deque" deque))
        ((empty-item? (second-of-deque deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-front-ptr! deque
                         (successor (front-of-deque deque)))
         (set-predecessor! (front-of-deque deque) '())))
  deque)

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE called with an empty deque" deque))
        ((empty-item? (second-of-deque deque))
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '()))
        (else
         (set-rear-ptr! deque
                        (predecessor (rear-of-deque deque)))
         (set-successor! (rear-of-deque deque) '())))
  deque)

;:*------------------------------------------------------------ abstract layer

;:*=======================
;:* applications
(define (push deque x)
  (rear-insert-deque! deque x))

(define (pop deque)
  (let ((x (content (front-of-deque deque))))
    (front-delete-deque! deque)
    x))

(define (rev-push deque x)
  (front-insert-deque! deque x))

(define (rev-pop deque)
  (let ((x (content (rear-of-deque deque))))
    (rear-delete-deque! deque)
    x))

(define (print-deque deque)
  (define (print-iter item next)
    (cond ((empty-item? item)
           (display ""))
          (else
           (display (content item))
           (print-iter (next item) next))))
  (print-iter (front-of-deque deque) successor)
  (newline))
