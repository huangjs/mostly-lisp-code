;;; Algorithm:

;; As in RPN, we scan the formula from left to right, processing each
;; operand and operator in order. However, we now have two stacks: one
;; for operands and another for operators. Then, we proceed as
;; follows:

;; If we see an operand, push it on the operand stack.
;; If we see an operator:
;; - While there’s an operator on top of the operator stack of
;;   precedence higher than or equal to that of the operator we’re
;;   currently processing, pop it off and apply it. (That is, pop the
;;   required operand(s) off the stack, apply the operator to them,
;;   and push the result back on the operand stack.)
;; - Then, push the current operator on the operator stack.
;; When we get to the end of the formula, apply any operators
;; remaining on the stack, from the top down. Then the result is the
;; only item left on the operand stack (assuming well-formed input).

;; UPDATE:
;; - now associativity is properly supported
;; - ^ added
;; - syntax change: F(x) => (F x)
;; - there's a hack:
;;   if the second element of an expression is not a operator,then it's a prefix expression
;; - unary operator and parentheses are supported by lisp reader

(defparameter *operators*
  '(+ - * / ^))

(defparameter *operator-priorities*
  '((+ :priority 3 :associativity :left :lisp-function +)
    (- :priority 3 :associativity :left :lisp-function -)
    (* :priority 4 :associativity :left :lisp-function *)
    (/ :priority 4 :associativity :left :lisp-function /)
    (^ :priority 5 :associativity :right :lisp-function expt)))

(defparameter *operand-stack* '())
(defparameter *operator-stack* '())

(defun initialize ()
  (setf *operator-stack* '()
        *operand-stack* '()))

(defun operator-p (op)
  (member op *operators*))

(defun operand-p (x)
  (not (operator-p x)))

(defun operator-properties (op)
  (cdr (assoc op *operator-priorities*)))

(defun operator-priority (op)
  (getf (operator-properties op) :priority))

(defun operator-associativity (op)
  (getf (operator-properties op) :associativity))

(defun operator-function (op)
  (getf (operator-properties op) :lisp-function))

;;;
(defun eval-prefix (exp)
  (print (list exp *operand-stack* *operator-stack*))
  (eval exp))

(defun pop-eval ()
  ;; listp => eval as prefix
  (let ((e (pop *operand-stack*)))
    (if (listp e)
        (eval-prefix e)
        e)))

(defun reduce-1 ()
  (let* ((op (pop *operator-stack*))
         (e2 (pop-eval))
         (e1 (pop-eval)))
    (push (eval-prefix (list (operator-function op) e1 e2)) *operand-stack*)))

(defun eval-expression (exp)
  (when (atom exp)
    (return-from eval-expression (eval-prefix exp)))
  ;;
  (initialize)
  (loop for e in exp
        do (shift e))
  ;; eval remaining 
  (labels ((reduce-remain ()
             (loop while *operator-stack*
                   do (let* ((op (pop *operator-stack*))
                             (e1 (pop-eval)))
                        (if (and *operator-stack*
                                 (or (and (eq :left (operator-associativity op))
                                          (<= (operator-priority op)
                                              (operator-priority (first *operator-stack*))))
                                     (and (eq :right (operator-associativity op))
                                          (< (operator-priority op)
                                             (operator-priority (first *operator-stack*))))))
                            (progn
                              (reduce-remain)
                              (push op *operator-stack*)
                              (push e1 *operand-stack*)
                              (reduce-1))
                            (progn
                              (push op *operator-stack*)
                              (push e1 *operand-stack*)
                              (reduce-1)
                              (reduce-remain)))))))
    (reduce-remain))
  (prog1
      (pop *operand-stack*)
    (when *operand-stack*
      (error "Invalid expression"))))

(defun eval-expression-with-new-environment (exp)
  (let ((*operand-stack* '())
        (*operator-stack* '()))
    (eval-expression exp)))

(defun shift (e)
  (cond ((listp e)
         (if (not (operator-p (second e)))
             ;; e.g. (sin 30)
             (push (cons (first e)
                         (mapcar #'eval-expression-with-new-environment (rest e)))
                   *operand-stack*)
             ;; e.g. (2 + 3)
             ;; FIXME: wrong evaluation order for right associative operators
             (push (eval-expression-with-new-environment e) *operand-stack*)))
        ((operator-p e)
         (when *operator-stack*
           (ecase (operator-associativity e)
             (:left
              (when (<= (operator-priority e)
                        (operator-priority (first *operator-stack*)))
                (reduce-1)))
             (:right
              (when (< (operator-priority e)
                       (operator-priority (first *operator-stack*)))
                (reduce-1)))))
         (push e *operator-stack*))
        (t
         (push e *operand-stack*))))

#| examples

 (eval-expression '(1 + 2 * 3 - 4)) => 3
 (eval-expression '(1 + 2 * 3 - 4 / 5 )) => 31/5
 (eval-expression '(1 + 2 * 3 - 4 / 5 + 6)) => 61/5

 (eval-expression '(3 + 4 * 2 / (1 - 5) ^ 2 ^ 3)) => 24577/8192
 (eval-expression '(3 + 4 * 2 / (sin (PI / 2)))) => 11.0d0

|#

