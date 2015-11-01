;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; Commented by Jianshi Huang
;;; Part of the comments are excerpted from PAIP.


;;; Compilation option
(proclaim '(optimize (debug 0) (safety 0) (space 0) (speed 3)))

;;; memoization
(defun memo (fn)
  "Return a function that computes the same results but with memoization."
  (let ((table (make-hash-table)))
    #'(lambda (x)                       ;return a function with closure!!!
        (multiple-value-bind (value found?)
            (gethash x table)
          (if found?
              value
              (setf (gethash x table)
                    (funcall fn x)))))))

;;; example
;;; fibonacci
;;; usage: (setf memo-fib (memo #'fib))
;;;		   (funcall memo-fib 5)
(defun fib (n)
  "Compute the nth number in the Fibonacci sequence."
  (if (<= n 1)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

;;; memoize
;;; Improved Version!!!
;;; usage: (memoize 'fib)
;;; 	   (fib 5)
(defun memoize (fn-name)
  "Replace fn-name's global definition with a memoized version."
  (setf (symbol-function fn-name) (memo (symbol-function fn-name))))

;;; hand optimized fib
(defun fib2 (n)
  (do ((a 1 b)
       (b 1 (+ a b))
       (m 0 (1+ m)))
      ((>= m n) a)))

;;;make a macro for easy define
;;; usage: (defun-memo f (x) ...)
;;; another reason we need this is that when we
;;; make a change to the function,  we need to recompile the original
;;; function and redo the call to memoize
(defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))


;;; Still has 3 problems.
;;; 1. it only works for functions of one argument.
;;; 2. it only returns a stored value for arguments that are EQL
;;; 3. it has no way to delete the entries from the hash table
;;; The version below handle these three problems.
;;; They are compatible with thte previous version
(defun memo (fn name key test)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)       ; use p-list to store the hash table, so that it could be cleared.
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (value found?)
              (gethash k table)
            (if found?
                value
                (setf (gethash k table) (apply fn args))))))))

;;; any combination of arguments as the key can be memoized.
;;; The default is to memoize only the first arguments
;;; If you want to use all the arguments, specify INDENTITY as the key.
(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name) fn-name key test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

