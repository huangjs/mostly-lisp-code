;;; Cooperative multi-threader

(define *threads* '())
(define *bottom* #f)

(define (push-thread! thread)
  (set! *threads* (append *threads* (list thread))))

(define (pop-thread!)
  (let ((thread (car *threads*)))
    (set! *threads* (cdr *threads*))
    thread))

(define (spawn thunk)
  (push-thread! (lambda (x) (thunk))))

(define (yield)
  (call-with-current-continuation (lambda (k)
                                    (push-thread! k)
                                    (schedule))))

(define (run thunk)
  (call-with-current-continuation (lambda (k)
                                    (set! *bottom* k)
                                    (spawn thunk)
                                    (schedule))))

(define (schedule)
  (if (null? *threads*)
      (*bottom*)
      (let ((thunk (pop-thread!)))
        (thunk #f)
        (schedule))))

