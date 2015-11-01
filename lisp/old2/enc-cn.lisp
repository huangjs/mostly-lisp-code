;;; Chun Tian (binghe) <binghe.lisp@gmail.com>
;;; Sat Dec 23 02:45:12 CST 2006

(in-package :sb-impl)

;;; GBK
(declaim (inline ucs-to-gbk gbk-to-ucs
                 mb-len-as-gbk gbk-continuation-byte-p))

(defun ucs-to-gbk (code)
  (declare (optimize speed (safety 0))
           (type fixnum code))
  (if (<= code #x7f)
	  code
      (get-multibyte-mapper *ucs-to-gbk-table* code)))

(defun gbk-to-ucs (code)
  (declare (optimize speed (safety 0))
           (type fixnum code))
  (if (<= code #x7f) code
      (get-multibyte-mapper *gbk-to-ucs-table* code)))

(defun mb-len-as-gbk (code)
  (declare (optimize speed (safety 0))
           (type (unsigned-byte 8) code))
  (if (< code #x81) 1 2))

(defun gbk-continuation-byte-p (code)
  (declare (optimize speed (safety 0))
           (type (unsigned-byte 8) code))
  (or (<= #x40 code #x7E) (<= #x80 code #xFE)))

(define-multibyte-encoding :gbk (:gbk :|GBK| :cp936 :|936|)
  ucs-to-gbk gbk-to-ucs mb-len-as-gbk gbk-continuation-byte-p)


