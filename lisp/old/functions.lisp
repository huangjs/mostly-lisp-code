;:*=======================
;:* optional parameter
;;; the height par will take the same value as width unless explicitly specified.
(defun make-rectangle (width &optional (height width))
  ...)

;:*=======================
;:* with specified-p fag
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

;:*=======================
;:* keyword pars
(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

;:*=======================
;:* return a value 
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        ;; (return (list i j)) which is the syntax sugar of return-from a nil block.
        (return-from foo (list i j))))))

;:*=======================
;:* funcall and apply
(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))

(plot (first plot-data) (second plot-data) (third plot-data) (fouth plot-data))
;;;equals to
(apply #'plot plot-data)

;:*=======================
;:* lambda
(defun double (x)
  (+ x x))

(funcall #'(lambda (x y) (+ x y)) 2 3) ; -> 5
((lambda (x y) (+ x y)) 2 3) ; -> 5
(funcall (lambda (x y) (+ x y)) 2 3) ; this is also legal in Common Lisp (historical reason)
