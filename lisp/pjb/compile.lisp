;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               compile.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Replaces the Makefile.
;;;;    
;;;;    Usage:   (load "compile.lisp")
;;;;
;;;;    will compile all outdated files.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-13 <PJB> Added generation of ASD file and use of ASDF.
;;;;    2004-07-23 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2004 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************

;; (defpackage "COM.INFORMATIMAGO.COMMON-LISP.COMPILE"
;;   (:use "COMMON-LISP")
;;   (:export "MAIN"))
;; (in-package "COM.INFORMATIMAGO.COMMON-LISP.COMPILE")


;;; Not used yet:
(DEFVAR *PREFIX* "/usr/local/")
(DEFVAR *MODULE* "common-lisp")
(DEFVAR *PACKAGE-PATH* "com/informatimago/common-lisp")
;;; ----


(LOAD "init.lisp")
;; package.lisp is loaded by init.lisp.
;;(package:load-package :com.informatimago.common-lisp.make-depends)
(SETF PACKAGE:*PACKAGE-VERBOSE* NIL)
;; Load make-depends dependencies:
(DEFVAR *MAKE-DEPENDS-DEPENDENCIES*
   '(SOURCE-FORM UTILITY ECMA048 LIST STRING CHARACTER-SETS
     ASCII STREAM FILE HTML MAKE-DEPENDS))
(DOLIST (FILE *MAKE-DEPENDS-DEPENDENCIES*)
  (LOAD (MAKE-PATHNAME
         :NAME (STRING FILE) :TYPE "LISP" :VERSION NIL :CASE :COMMON
         :DEFAULTS (or *LOAD-PATHNAME* *DEFAULT-PATHNAME-DEFAULTS*))))
(LOAD "PACKAGES:NET;SOURCEFORGE;CCLAN;ASDF;ASDF.LISP")
(PUSH (FUNCTION PACKAGE:PACKAGE-SYSTEM-DEFINITION)
      ASDF:*SYSTEM-DEFINITION-SEARCH-FUNCTIONS*)


(DEFPARAMETER *SOURCES*
  '(
	PACKAGE 

	SOURCE-FORM                     ; used by READER and UTILITY, etc.
    READER                          ; used by SOURCE-TEXT
	SOURCE-TEXT

	UTILITY
    ASCII                               ; = iso646-006 (US)
    ECMA048                             ; = iso6429
	LIST
    DLL
	QUEUE
    ARRAY
	STRING 
	STREAM
    
    FILE ;; file uses stream
	PEEK-STREAM

    SCANNER
    PARSER 

	AVL     
	BSET     
	BRELATION 
	DICTIONARY 
	GRAF 
	GRAPH 
	
	GRAPH-DOT    
	GRAPH-DIAGRAM 

	COMBINATION
    
	PMATCH 
	PICTURE  

    MEMORY
    HEAP
	ACTIVITY  
    MESSAGE-QUEUE

	FLOAT-BINIO  
	DATA-ENCODING 
    CONS-TO-ASCII
	TREE-TO-ASCII  
	TREE-TO-DIAGRAM 
	REGEXP-POSIX
	REGEXP-EMACS

	RFC2822
    RFC3548
	ISO639A 
	ISO3166
    ISO4217
    
    CHARACTER-SETS ; (previously IATA-CHARACTER-SETS) with implementation specific stuff..

    HTML-ISO8879-1 
	HTML
    HQUERY
	HTRANS
	DATABASE 
	
    PARSE-HTML
    CACHE                               ; a generic disk-based cache.

	ALIASES 
	PASSWD 
    GROUP
	PRIMES 
    TEA
    RAIDEN
	MAKE-DEPENDS 
    CXX                   ; Simple C++ Parser for call graph analysis.

	CSV                             ; Coma-Separated-Values files.
    IBAN                            ; Internation Bank Account Number.
    RIB                             ; Relevés d'Identité Bancaires.
    
    INVOICE            ; my personal accounting and invoicing package.
    
	BROWSER  ; a file browser (and cd/pwd/pushd/popd/ls/cat/more cmds.

	ED                        ; a simple editor, for 
                                        ; common-lisp implementations
                                        ; lacking a COMMON-LISP:EDIT function...

    INTERACTIVE
    )) 

(DEFPARAMETER *SOURCE-TYPE* "lisp")


(DEFUN VERSION++ (&OPTIONAL PATH)
  "
DO:      Increment the version compilation number.
         The version is persistent, stored in a file named VERSION.DAT
         in the same directory as *LOAD-PATHNAME*, or at PATH.
RETURN:  The version as a string \"major.minor.compilation\"
"
  (FLET ((READ-VERSION (FILE)
           (LOOP
              :FOR LINE = (READ-LINE FILE NIL NIL)
              :FOR =POS = (WHEN LINE (POSITION (CHARACTER "=") LINE))
              :WHILE LINE
              :WHEN =POS
              :COLLECT (LIST (INTERN (STRING-UPCASE (SUBSEQ LINE 0 =POS)) "KEYWORD")
                             (READ-FROM-STRING (SUBSEQ LINE (1+ =POS)))))))
    (LET* ((DEFAULT-PATH       (OR *LOAD-PATHNAME* *DEFAULT-PATHNAME-DEFAULTS*))
           (VERSION.PATH       (OR PATH 
                                   (MAKE-PATHNAME :NAME "VERSION" :TYPE "DAT"
                                                  :VERSION :NEWEST
                                                  :DEFAULTS DEFAULT-PATH)))
           (VERSION             (WITH-OPEN-FILE (FILE VERSION.PATH
                                                      :DIRECTION :INPUT
                                                      :IF-DOES-NOT-EXIST :ERROR)
                                  (READ-VERSION FILE)))
           (VERSION.MAJOR           (OR (SECOND (ASSOC :MAJOR       VERSION)) 0))
           (VERSION.MINOR           (OR (SECOND (ASSOC :MINOR       VERSION)) 0))
           (VERSION.COMPILATION (1+ (OR (SECOND (ASSOC :COMPILATION VERSION)) 0)))
           (NEW-VERSION `((:MAJOR       ,VERSION.MAJOR)
                          (:MINOR       ,VERSION.MINOR)
                          (:COMPILATION ,VERSION.COMPILATION))))
      (WITH-OPEN-FILE (FILE VERSION.PATH
                            :DIRECTION :OUTPUT
                            :IF-DOES-NOT-EXIST :CREATE
                            :IF-EXISTS :SUPERSEDE)
        (FORMAT FILE "~(~:{~A=~A~%~}~)" NEW-VERSION))
      (VALUES (FORMAT NIL "~A.~A.~A"
                      VERSION.MAJOR VERSION.MINOR VERSION.COMPILATION)
              VERSION.MAJOR VERSION.MINOR VERSION.COMPILATION))))

 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generate the asdf system file, loading the sources.

(format *trace-output* "~2%;;;; GENERATING THE ASDF SYSTEM FILE~2%")

(handler-bind ((warning #'muffle-warning))
  (COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS:GENERATE-ASD
   :COM.INFORMATIMAGO.COMMON-LISP *SOURCES* *SOURCE-TYPE*
   :VERSION (VERSION++)
   :IMPLICIT-DEPENDENCIES '("package")))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cleanup before asdf:load-op:
;;; we delete the package to let asdf:load-op load them cleanly.
;;;

(format *trace-output* "~2%;;;; CLEANING THE LOADED PACKAGES~2%")

(DEFUN PACKAGE-USE*-PACKAGE-P (P Q)
  "
RETURN: Whether the package P uses the package Q, or a package 
        that uses the package Q.
NOTE:   By definition, (PACKAGE-USE*-PACKAGE-P X X)
"
  (SETF P (FIND-PACKAGE P)
        Q (FIND-PACKAGE Q))
  (LOOP
     :WITH PROCESSED = '()
     :WITH USED = (LIST P)
     :WHILE USED
     ;; :do (print (list used processed))
     :DO (LET ((CURRENT (POP USED)))
           (IF (EQ CURRENT Q)
               (RETURN-FROM PACKAGE-USE*-PACKAGE-P T)
               (PROGN
                 (PUSH CURRENT PROCESSED)
                 (DOLIST (NEW (PACKAGE-USE-LIST CURRENT))
                   (UNLESS (MEMBER NEW PROCESSED)
                     (PUSHNEW NEW USED))))))
     :FINALLY (RETURN-FROM PACKAGE-USE*-PACKAGE-P NIL)))


(DEFUN TOPOLOGICAL-SORT (NODES LESSP)
  "
RETURN: A list of NODES sorted topologically according to 
        the partial order function LESSP.
        If there are cycles (discounting reflexivity), 
        then the list returned won't contain all the NODES.
"
  (LOOP
     :WITH SORTED = '()
     :WITH INCOMING = (MAP 'VECTOR (LAMBDA (TO)
                                     (LOOP
                                        :FOR FROM :IN NODES
                                        :WHEN (AND (NOT (EQ FROM TO))
                                                   (FUNCALL LESSP FROM TO))
                                        :SUM 1))
                           NODES)
     :WITH Q = (LOOP
                  :FOR NODE :IN NODES
                  :FOR INCO :ACROSS INCOMING
                  :WHEN (ZEROP INCO)
                  :COLLECT NODE) 
     :WHILE Q
     :DO (LET ((N (POP Q)))
           (PUSH N SORTED)
           (LOOP
              :FOR M :IN NODES
              :FOR I :FROM 0
              :DO (WHEN (AND (AND (NOT (EQ N M))
                                  (FUNCALL LESSP N M))
                             (ZEROP (DECF (AREF INCOMING I))))
                    (PUSH M Q))))
     :FINALLY (RETURN (NREVERSE SORTED))))


;; (defun print-graph (nodes edge-predicate)
;;   (flet ((initiale (package)
;;            (if (< (length "COM.INFORMATIMAGO.COMMON-LISP.")
;;                   (length (package-name package)))
;;                (subseq (package-name package)
;;                        (length "COM.INFORMATIMAGO.COMMON-LISP.")
;;                        (1+ (length "COM.INFORMATIMAGO.COMMON-LISP.")))
;;                (subseq (package-name package) 0 1))))
;;     (let* ((nodes (coerce nodes 'vector))
;;            (width (ceiling (log (length nodes) 10))))
;;       (loop
;;          :for i :from 0
;;          :for node :across nodes
;;          :initially (format t "~2%")
;;          :do (format t " ~VD: ~A~%" width i node)
;;          :finally (format t "~2%"))
;;       (loop
;;          :for j :from 0 :below (length nodes)
;;          :initially (format t " ~VD " width "")
;;          :do (format t " ~VD" width j)
;;          :finally (format t "~%"))
;;       (loop
;;          :for i :from 0 :below (length nodes)
;;          :do (loop
;;                 :for j :from 0 :below (length nodes)
;;                 :initially (format t "~A ~VD:"  (initiale (aref nodes i)) width i)
;;                 :do (format t " ~VD"
;;                             width
;;                             (if (funcall edge-predicate
;;                                          (aref nodes i) (aref nodes j))
;;                                 (concatenate 'string
;;                                   (initiale (aref nodes i))
;;                                   (initiale (aref nodes j)))
;;                                  ""))
;;                 :finally (format t "~%"))
;;          :finally (format t "~%")))))



;;; With topological-sort, we mustn't use a total order function like this one:
;; (defun package<= (p q)
;;   (cond ((eq p q) t)
;;         ((package-use*-package-p p q)
;;          (assert (not (package-use*-package-p q p))
;;                  (p q) "A circle could happen but it should not.")
;;          t)                                ; p<q
;;         ((package-use*-package-p q p) nil) ; p>q
;;         (t (string<= (package-name p) (package-name q)))))



(DOLIST (P (LET* ((NODES
                    (DELETE-IF-NOT
                     (LAMBDA (P)
                       (LET ((PREFIX "COM.INFORMATIMAGO.COMMON-LISP."))
                         (AND (<  (LENGTH PREFIX) (LENGTH (PACKAGE-NAME P)))
                              (STRING= PREFIX (PACKAGE-NAME P)
                                       :END2 (LENGTH PREFIX)))))
                     (COPY-LIST (LIST-ALL-PACKAGES))))
                   (SORTED
                    (TOPOLOGICAL-SORT NODES
                                      (FUNCTION PACKAGE-USE*-PACKAGE-P)))
                   (CYCLIC (SET-DIFFERENCE NODES SORTED)))
              (WHEN CYCLIC
                (FORMAT T "Cyclic nodes = ~S~%" CYCLIC))
              (NCONC CYCLIC SORTED)))
  (DELETE-PACKAGE P))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now, we compile and load the system
;;;

(format *trace-output* "~2%;;;; COMPILING THE ASDF SYSTEM~2%")
(SETF ASDF:*COMPILE-FILE-WARNINGS-BEHAVIOUR* :IGNORE)
(let ((*LOAD-VERBOSE* t)
      (*COMPILE-VERBOSE* t)
      (asdf::*verbose-out* t))
  (ASDF:OPERATE 'ASDF:LOAD-OP :COM.INFORMATIMAGO.COMMON-LISP))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finally, we generate a summary.html page.
;;;
(format *trace-output* "~2%;;;; GENERATING THE SUMMARY.HTML~2%")
(handler-bind ((warning #'muffle-warning))
  (COM.INFORMATIMAGO.COMMON-LISP.MAKE-DEPENDS:GENERATE-SUMMARY
   *SOURCES*
   :VERBOSE nil
   :SOURCE-TYPE *SOURCE-TYPE*
   :SUMMARY-PATH "summary.html"
   :character-set "UTF-8"
   :REPOSITORY-URL (LAMBDA (PP)
                     (FORMAT NIL "http://darcs.informatimago.com~
                                 /darcs/public/lisp/~(~A/~A~).lisp"
                             (CAR (LAST (PATHNAME-DIRECTORY PP)))
                             (PATHNAME-NAME PP)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;