;;; Example persistent hash tables via shallow binding.
;;; See <http://www.pvk.ca/Blog/Lisp/persistent_dictionary.html>

;;; License: Modified BSD
;;; License Copyright (c) 2008, Paul-Virak Khuong
;;;  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; Redistributions of source code must retain the above copyright
;;; notice, this list of conditions and the following disclaimer.
;;; Redistributions in binary form must reproduce the above copyright
;;; notice, this list of conditions and the following disclaimer in the
;;; documentation and/or other materials provided with the distribution.
;;;
;;; Neither the name of the Million Monkey Enterprises nor the names of
;;; its contributors may be used to endorse or promote products derived
;;; from this software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defconstant +unbound+ '+unbound+)

(defstruct ptable
  (key        +unbound+
              :read-only t)
  (prev-value +unbound+)
  (next       (make-hash-table)))

(defun table-value (table key)
  (gethash key table +unbound+))

(defun (setf table-value) (value table key)
  (if (eq value +unbound+)
      (remhash key table) ; just assume that key = +unbound+ never happens
      (setf (gethash key table) value))
  value)

(declaim (inline reverse-ptable-list undo-changes))
(defun reverse-ptable-list (ptable-list)
  (declare (type ptable ptable-list)
           (optimize speed))
  (labels ((inner (list next)
             (if (ptable-p list)
                 (inner (shiftf (ptable-next list) next)
                        list)
                 (values next list))))
    (multiple-value-bind (reversed-list terminal)
        (inner ptable-list ptable-list)
      (setf (ptable-next ptable-list) terminal)
      (values reversed-list terminal))))

(defun undo-changes (change-list table)
  (declare (type ptable change-list)
           (type hash-table table)
           (optimize speed))
  (do ((change change-list next)
       (next   (ptable-next change-list)
               (ptable-next next)))
      ((not (ptable-p next))
       table)
    (rotatef (ptable-prev-value change)
             (table-value table
                          (ptable-key change)))))

(defun reroot (ptable)
  (declare (type ptable ptable))
  (when (ptable-p (ptable-next ptable))
    (multiple-value-call #'undo-changes
      (reverse-ptable-list ptable)))
  ptable)

(declaim (inline %set %get))
(defun %set (value ptable key)
  (declare (type ptable ptable)
           (optimize speed))
  (let* ((table      (ptable-next ptable))
         (new-ptable (make-ptable :next       table
                                  :key        key
                                  :prev-value value)))
    (rotatef (ptable-prev-value new-ptable)
             (table-value table key))
    (setf (ptable-next ptable) new-ptable)
    new-ptable))

(defun %get (ptable key)
  (declare (type ptable ptable))
  (gethash key (ptable-next ptable)))

(declaim (sb-ext:maybe-inline ptable-get ptable-set))
(defun ptable-set (value ptable key)
  (declare (type ptable ptable))
  (reroot ptable)
  (%set value ptable key))

(defun ptable-get (ptable key)
  (declare (type ptable ptable))
  (reroot ptable)
  (%get ptable key))

(defun test-hash-table (n)
  (let ((table (make-hash-table)))
    (time (dotimes (i n)
            (setf (gethash i table) i)))
    (time (dotimes (i 10)
            (dotimes (i n)
              (gethash i table))))))

(defun test-ptable (n)
  (declare (inline ptable-get))
  (let ((table (make-ptable)))
    (time (dotimes (i n)
            (setf table (ptable-set i table i))))
    (time (dotimes (i 10)
            (dotimes (i n)
              (ptable-get table i))))))
