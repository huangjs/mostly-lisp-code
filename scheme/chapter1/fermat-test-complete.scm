;:*=======================
;:* #t means n is a prime and has passed the fermat test.
(define (fermat-test-complete n)
  (define (test-congruent a n)
    (= (expmod a n n) a))
  (define (fermat-test-iter a n)
    (cond ((= a n) #t)
          ((test-congruent a n) (fermat-test-iter (+ a 1) n))
          (else (not #t))))
  (fermat-test-iter 1 n))


(define (find-carmichael-numbers begin end)
  (cond ((= begin end) '())
        ((and (fermat-test-complete begin)
              (not (prime? begin))) (cons begin
                                          (find-carmichael-numbers (+ 1 begin) end)))
        (else (find-carmichael-numbers (+ 1 begin) end))))
