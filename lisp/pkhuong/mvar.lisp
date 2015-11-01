;;; License: Modified BSD
;;; License Copyright (c) 2010, Paul-Virak Khuong
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

(defpackage "MVAR"
    (:use "CL" "SB-THREAD")
  (:export "MVAR" "MVAR-P" "MAKE" "VALUE" "TAKE" "PUT"))

(in-package "MVAR")
(defconstant +empty+ '+empty+)

(defstruct (mvar
             (:constructor %make-mvar))
  (mutex      (make-mutex)     :type mutex
   :read-only t)
  ;; signaled when reads are possible
  (read-cvar  (make-waitqueue) :type waitqueue
   :read-only t)
  ;; signaled when writes are possible
  (write-cvar (make-waitqueue) :type waitqueue
   :read-only t)
  (value  +empty+))

(defun take (mvar)
  (declare (type mvar mvar))
  (let ((mutex (mvar-mutex mvar))
        (cvar  (mvar-read-cvar mvar)))
    (with-mutex (mutex)
      (loop for value = (mvar-value mvar)
            do (cond ((eq value +empty+)
                      (condition-wait cvar mutex))
                     (t
                      (setf (mvar-value mvar) +empty+)
                      (condition-notify (mvar-write-cvar mvar))
                      (return value)))))))

(defun put (mvar new-value)
  (declare (type mvar mvar))
  (assert (not (eq new-value +empty+)))
  (let ((mutex (mvar-mutex mvar))
        (cvar  (mvar-write-cvar mvar)))
    (with-mutex (mutex)
      (loop for value = (mvar-value mvar)
            do (cond ((eq value +empty+)
                      (setf (mvar-value mvar) new-value)
                      (condition-notify (mvar-read-cvar mvar))
                      (return new-value))
                     (t
                      (condition-wait cvar mutex)))))))

(declaim (inline value (setf value)))
(defun value (mvar)
  (take mvar))
(defun (setf value) (value mvar)
  (put mvar value))

(defun make (&optional (value +empty+))
  (%make-mvar :value value))
