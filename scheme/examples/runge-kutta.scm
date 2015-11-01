(define (runge-kutta as bs cs)
    (lambda (y0 t0 f h)
      (let ((ts (stream-map (lambda (c)
                              (+ t0
                                 (* c h)))
                            cs))
            (ks '(0)))
        (define ks (stream-map f
                               ts
                               (stream-map (lambda (x)
                                             (accumulate +
                                                         y0
                                                         (stream-scale (stream-mul x
                                                                                   ks)
                                                                       h)))
                                           as)))
        (+ y0 (* h
                 (accumulate +
                             0
                             (stream-mul bs ks)))))))


(define rk4
    (let ((bs '(1/6 1/3 1/3 1/6))
          (cs '(0 1/2 1/2 1))
          (as '((0)
                (1/2)
                (0 1/2)
                (0 0 1))))
      (runge-kutta as bs cs)))

(define f
    (lambda (t y)
      (+ (tan y)
         1)))
