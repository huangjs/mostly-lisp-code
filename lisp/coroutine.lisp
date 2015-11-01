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

(defpackage "COROUTINE"
    (:use "CL" "SB-THREAD" "SB-EXT")
  (:export "YIELD" "COROUTINE" "NEXT" "+DEAD+"))
(in-package "COROUTINE")
(defstruct (coroutine
             (:constructor %make-coroutine (thread in out)))
  (thread nil :type thread    :read-only t)
  (in     nil :type mvar:mvar :read-only t)
  (out    nil :type mvar:mvar :read-only t))

(defun next (coroutine &rest values)
  (mvar:put (coroutine-in coroutine) values)
  (values-list (mvar:take (coroutine-out coroutine))))

(defun yield (&rest values)
  (declare (ignore values))
  (error "~S used outside ~S" 'yield 'coroutine))

(define-compiler-macro yield (&whole whole &rest values)
  (declare (ignore values))
  (warn "~S used outside ~S" 'yield 'coroutine)
  whole)

(defun make-coroutine (builder)
  (let* ((in     (mvar:make))
         (out    (mvar:make))
         (thread (make-thread (funcall builder in out)))
         (coroutine (%make-coroutine thread in out)))
    ;; the coroutine thread and the finalizer don't hold references to
    ;; the coroutine struct, so finalize isn't useless.
    (finalize coroutine
                     (lambda ()
                       (mvar:put in +dead+)
                       (join-thread thread)))
    ;; wait for the first (bogus) return value: the coroutine has been fully
    ;; initialized
    (mvar:take out)
    coroutine))

(defconstant +dead+ '+dead+)

(defmacro coroutine (&body body)
  (let ((_in    (gensym "IN"))
        (_out   (gensym "OUT"))
        (_block (gensym "BLOCK")))
    `(make-coroutine
      (lambda (,_in ,_out)
        "IN is the input MVAR and OUT the output MVAR."
        (lambda ()
          (block ,_block
            (flet ((yield (&rest values)
                     (mvar:put ,_out values)
                     (let ((in (mvar:take ,_in)))
                       (when (eq in +dead+)
                         (return-from ,_block))
                       (values-list in))))
              ;; signal that initialization is complete
              (yield)
              (locally
                  ,@body)))
          (mvar:put ,_out (list +dead+)))))))
