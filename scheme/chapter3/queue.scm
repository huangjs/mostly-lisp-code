(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

;:*------------------------------------------------------------ abstract layer

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-of-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

;:*=======================
;:* queue without the front item
(define (rest-of-queue queue)
  (define q (make-queue))
  (cond ((empty-queue? queue)
         (error "the queue is empty" queue))
        (else
         (set-front-ptr! q (cdr (front-ptr queue)))
         (set-rear-ptr! q (rear-ptr queue))))
  q)

;:*=======================
;:* for double-ended queue
(define (rear-of-queue queue)
  (if (empty-queue? queue)
      (error "REAR called with an empty queue" queue)
      (car (rear-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DEQUEUE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;:*=======================
;:* application
(define (print-queue queue)
  (cond ((empty-queue? queue) (display ""))
        (else
         (display (front-of-queue queue))
         (print-queue (rest-of-queue queue)))))
