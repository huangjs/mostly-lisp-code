(in-package :cl-user)

(defun square (x)
  (* x x))

(defun power (x n)
  (cond ((= n 0)
         1)
        ((oddp n)
         (* x (power x (- n 1))))
        (t
         (square (power x (/ n 2))))))

;; (funcall (lambda () (power 3 6)))
;; (funcall (lambda (x) (power x 6)) 3)
;; (funcall (lambda (x n) (power x n)) 3 6)

;;; define-compiler-macro approach
(define-compiler-macro power (&whole form x n)
  (if (not (constantp n))
      form
      (if (constantp x)
          (expt x n)
          (cond ((= n 0)
                 1)
                ((oddp n)
                 `(* ,x (power ,x ,(- n 1))))
                (t
                 `(square (power ,x ,(/ n 2))))))))

;;; define-compiler-macro with &environment
(define-compiler-macro power (&whole form x n &environment env)
  (if (not (constantp n env))
      form
      (let ((n (if (symbolp n)
                   (symbol-value n)
                   n)))
        (if (constantp x env)
            (let ((x (if (symbolp x)
                         (symbol-value x)
                         x)))
              (expt x n))
            (cond ((= n 0)
                   1)
                  ((oddp n)
                   `(* ,x (power ,x ,(- n 1))))
                  (t
                   `(square (power ,x ,(/ n 2)))))))))

;;; macrolet approach
(defun emit-* (x y)
  (cond ((or (eql x 0) (eql y 0)) 0) 
        ((eql y 1) x)
        ((eql x 1) y)
        ((and (equal x y)
              (not (symbolp x)))
         `(let ((y ,x))
            (* y y)))
        (t
         `(* ,x ,y))))

(defun emit-power (x n)
  (locally
      (declare (disable-package-locks cl:*))
    (macrolet ((* (&rest a)
                 `(emit-* ,@a)))
      ;; original code
      (labels ((square (x)
                 (* x x))
               (power (x n)
                 (cond ((= n 0) 1)
                       ((oddp n) (* x (power x (- n 1))))
                       (t (square (power x (/ n 2)))))))
        (power x n)))))

;;; partial evaluation approach

;; (mix '(if (> x 2) 'more 'less) '((x . 4)))
;; (mix '(let ((x y) (z (1+ y))) (* x z)) '((y . 3)))
;; (fnmix 'power '(x 3))
;; (specialize 'power '(x 3) 'cube)
;; (fndef 'cube)
;; (cube 4)
;; (fnmix 'power '(x 22))
;; (fnmix 'power '(2 10))

