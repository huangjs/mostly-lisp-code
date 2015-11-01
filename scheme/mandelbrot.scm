
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the Bigloo Mandelbrot benchmark  as follows:
;;
;; bigloo -Obench mandelbrot.scm
;; time ./a.out 400
;;
;; Important note: the code only works in -Obench mode.
;; Do not be worried: it is legal Scheme/Bigloo code.
;; However, the 'multiple-value-bind' construct won't work
;; in ordinary mode, e.g: 'bigloo mandelbrot'. The latter
;; will raise a runtime error. Although the developers of
;, Bigloo told me they will change that in one of the next
;; Bigloo releases so as to have 'multiple-value-bind'
;; constructs also in debugging mode.
;;
;; Second note: At least on my Mac OSX Macintosh ibook
;; my Bigloo version is within 2 times the C code
;; (for C codes consult language shootout-pages):
;;
;; C code: real: 0m0.8s, user: 0m0.12s, sys: 0m0.05s
;;
;; Bigloo code: real: 0m0.9s, user: 0m0.24s, sys: 0m0.05s
;;
;;
;; Third note: Bigloo has some basic methods for handling
;; bit operations, e.g: (bit-or (bit-lsh byte-acc 1) 0)
;;
;; Fourth note: the code utilizes Bigloo its wonderful
;; built in support of operators meant for dealing with
;; numbers, e.g. (+fl x y) will typically boost things
;; a little bit because the compiler knows in advance
;; with which types he will have to cope with.
;;
;; Fifth note: As a Scheme programmer not used to Bigloo
;; don't become distracted by my type information in my code.
;; A proper syntax highlightning (e.g. in Bee) will
;; reveal the genuine Scheme code.
;;
;; Suggestions and complaints are welcome. Do not hesitate.
;;
;; (C) 2005 here is my name, <and here my email-adress@edu.>,
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module mandelbrot
   (main main))

(define *cflags* "-Wall -O3 -fomit-frame-pointer -mfpmath=sse -mtune=core2 -lm")

(define (main argv)
   (mandelbrot (string->number
                (car (cdr argv)))))

(define (mandelbrot::bool w::int)
   (let* ((iter::int 50)
          (limit::double 2.0)
          (h::int w))
      (print "P4 ")
      (print w " " h)
      (let >loop-y< ((y::int 0) (bit-num::ubyte 0) (byte-acc::ubyte 0))
           (cond
              ((>=fx y h) #t)
              (else
               (let >loop-x< ((x::int 0) (bit-num::ubyte bit-num) (byte-acc::ubyte byte-acc))
                    (cond
                       ((>=fx x w)
                        (>loop-y< (+fx y 1) bit-num byte-acc))
                       (else
                        (let* ((cr::double (-fl (*fl 2.0
                                                     (/fl (fixnum->flonum x) (fixnum->flonum w)))
                                                1.5))
                               (ci::double (-fl (*fl 2.0
                                                     (/fl (fixnum->flonum y) (fixnum->flonum h)))
                                                1.0)))
                           (let >loop-i< ((i::int 0) (zr::double 0.0) (zi::double 0.0))
                                (cond
                                   ((or (>=fx i iter)
                                        (>fl (+fl (*fl zr zr)
                                                  (*fl zi zi))
                                             (*fl limit limit)))
                                    (multiple-value-bind (bit-num-new byte-acc-new)
                                       (putc zr zi limit bit-num byte-acc w x)
                                       (>loop-x< (+fx x 1) bit-num-new byte-acc-new)))
                                   (else
                                    (let* ((tr::double (+fl (-fl (*fl zr zr)
                                                                 (*fl zi zi))
                                                            cr))
                                           (ti::double (+fl (*fl 2.0
                                                                 (*fl zr zi))
                                                            ci)))
                                       (>loop-i< (+fx i 1) tr ti))))))))))))))

(define (putc::obj zr::double zi::double limit::double bit-num::int byte-acc::int w::int x::int)
   (let* ((byte-acc-new::int (if (>fl (+fl (*fl zr zr)
                                            (*fl zi zi))
                                       (*fl limit limit))
                                  (bit-or (bit-lsh byte-acc 1) 0)
                                  (bit-or (bit-lsh byte-acc 1) 1)))
          (bit-num-new::int (+fx bit-num 1)))
      (if (=fx bit-num-new 8)
          (begin
             (display (integer->char byte-acc-new))
             (values 0 0))
          (begin
             (if (=fx x (-fx w 1))
                 (begin
                    (display (integer->char (bit-lsh byte-acc-new
                                                     (-fx (modulo w 8) 8))))
                    (values 0 0))
                 (begin
                    (values  bit-num-new byte-acc-new))))))) 
