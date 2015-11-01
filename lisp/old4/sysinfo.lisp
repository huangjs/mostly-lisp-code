;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; sysinfo.lisp --- FFI definitions for GetSystemInfo().
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;(in-package #:trivial-features-tests)

(defctype word :unsigned-short)
(defctype dword :unsigned-long)
(defctype pvoid :pointer)

(defcenum (architecture word)
  (:amd64 9)
  (:ia64 6)
  (:intel 0)
  (:unknown #xffff))

(defcstruct sysinfo-anon-struct
  (processor-architecture architecture)
  (reserved word))

(defcunion sysinfo-anon-union
  (oem-id dword)
  (anon-struct sysinfo-anon-struct))

(defcstruct system-info
  (anon-union sysinfo-anon-union)
  (page-size dword)
  (minimum-application-address pvoid)
  (maximum-application-address pvoid)
  (active-processor-mask dword)
  (number-of-processors dword)
  (processor-type dword)
  (allocation-granularity dword)
  (processor-level word)
  (processor-revision word))

(load-foreign-library "kernel32.dll")

(defcfun ("GetSystemInfo" %get-system-info :cconv :stdcall) :void
  (system-info :pointer))

(defun get-system-info ()
  (with-foreign-object (si 'system-info)
    (%get-system-info si)
    (foreign-slot-value (foreign-slot-value
                         (foreign-slot-value si 'system-info 'anon-union)
                         'sysinfo-anon-union 'anon-struct)
                        'sysinfo-anon-struct 'processor-architecture)))
