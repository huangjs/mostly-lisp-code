;:*=======================
;:* from the book
(define total_kinds 6)

;(define (count-change amount)
;  (cc amount total_kinds))

;(define (cc amount kinds)
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (= kinds 0)) 0)
;        (else (+ (cc amount
;                     (- kinds 1))
;                 (cc (- amount
;                        (get-value kinds))
;                     kinds)))))

(define (get-value kind)
  (cond ((= kind 1) 1)
        ((= kind 2) 5)
        ((= kind 3) 10)
        ((= kind 4) 50)
        ((= kind 5) 100)
        ((= kind 6) 500)
        (else 99999999)))     ; make it impossible       


;:*=======================
;:* my definition
(define (count-change amount)
  (count-iter amount 1))

(define (count-iter amount cur-kind)
  (define cur-value (get-value cur-kind))
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (> cur-kind total_kinds))
         0)
        (else (+ (count-iter (- amount cur-value) cur-kind)
           (count-iter amount (+ 1 cur-kind))))))
