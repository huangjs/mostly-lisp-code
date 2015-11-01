(defpackage :double-array
  (:use :cl))

(in-package :double-array)

;;;;
(defstruct tripple-array
  base
  check)

;;;;
(defvar .fail. (gensym "FAIL"))
(declaim (inline fail fail-p succeed-p))
(defun fail ()
  '#..fail.)
(defun fail-p (result)
  (eq result '#..fail.))
(defun succeed-p (result)
  (not (eq result '#..fail.)))

