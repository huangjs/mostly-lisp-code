;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package exports some file utility functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2007-07-07 <PJB> Made use of new CONTENTS-FROM-STREAM function.
;;;;    2006-08-05 <PJB> Added SAFE-TEXT-FILE-TO-STRING-LIST.
;;;;    2005-03-17 <PJB> Added REMOVE-FIRST-LINES.
;;;;    2005-02-20 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2005 - 2007
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

(IN-PACKAGE "COMMON-LISP-USER")
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.ASCII"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.FILE"
  (:DOCUMENTATION
   "This package exports file utility functions.

    Copyright Pascal J. Bourguignon 2005 - 2007
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:USE "COMMON-LISP" "COM.INFORMATIMAGO.COMMON-LISP.STREAM")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.STREAM"
                "CONTENTS-FROM-STREAM"
                "STREAM-TO-STRING-LIST" "COPY-STREAM" "COPY-OVER")
  (:EXPORT "REMOVE-FIRST-LINES" "BINARY-FILE-CONTENTS"
           "SAFE-TEXT-FILE-TO-STRING-LIST" "TEXT-FILE-TO-STRING-LIST"
           "TEXT-FILE-CONTENTS" "SEXP-FILE-CONTENTS" "COPY-FILE"))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.FILE")


(defun copy-file (src dst &key (if-exists :error) (external-format :default)
                  (element-type 'character))
  "
DO:     Copy the contents of the file at path SRC to the file at path DST.
"
  (with-open-file (inp src
                       :direction :input
                       :if-does-not-exist :error
                       :external-format external-format
                       :element-type element-type)
    (with-open-file (out dst
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists if-exists
                         :external-format external-format
                         :element-type element-type)
      (copy-stream inp out))))


(defun SEXP-FILE-CONTENTS (path &key (if-does-not-exist :error)
                           (external-format :default))
  "
RETURN: The contents of the file at PATH as a LIST of SEXP.
        or what is specified by IF-DOES-NOT-EXIST if it doesn't exist.
"
  (with-open-file (in path :direction :input
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format)
    (if (streamp in)
        (loop
           :for sexp = (read in nil in)
           :until (eq sexp in)
           :collect sexp)
        in)))


(DEFUN TEXT-FILE-TO-STRING-LIST (PATH &key (if-does-not-exist :error)
                           (external-format :default))
  "
RETURN:  the list of lines collected from the file.
"
  (with-open-file (in path :direction :input
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format)
    (STREAM-TO-STRING-LIST  in)))


(defun TEXT-FILE-CONTENTS (path &key (if-does-not-exist :error)
                           (external-format :default))
  "
RETURN: The contents of the file at PATH as a LIST of STRING lines.
        or what is specified by IF-DOES-NOT-EXIST if it doesn't exist.
"
  (with-open-file (in path :direction :input
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format)
    (if (streamp in)
        (contents-from-stream in :min-size 16384)
        in)))


(defun (setf text-file-contents) (new-contents path
                                  &key (if-does-not-exist :create)
                                  (if-exists :supersede)
                                  (external-format :default))
  "
RETURN: The NEW-CONTENTS, or if-exists or if-does-not-exist in case of error.
DO:     Store the NEW-CONTENTS into the file at PATH.  By default,
        that file is created or superseded; this can be changed with
        the keyword IF-DOES-NOT-EXIST or IF-EXISTS.
"
  (with-open-file (out path :direction :output
                       :if-does-not-exist if-does-not-exist
                       :if-exists if-exists
                       :external-format external-format)
    (if (streamp out)
        (write-sequence new-contents out)
        out)))


(defun BINARY-FILE-CONTENTS (path &key (if-does-not-exist :error)
                             (ELEMENT-TYPE '(UNSIGNED-BYTE 8))
                             (external-format :default))
  "
RETURN: The contents of the file at PATH as a VECTOR of (UNSIGNED-BYTE 8),
        or what is specified by IF-DOES-NOT-EXIST if it doesn't exist.
"
  (with-open-file (in path :direction :input
                      :if-does-not-exist if-does-not-exist
                      :ELEMENT-TYPE ELEMENT-TYPE
                      :external-format external-format)
    (if (streamp in)
        (contents-from-stream in :min-size 16384)
        in)))



(defun SAFE-TEXT-FILE-TO-STRING-LIST (PATH &key (if-does-not-exist :error))
  "
DO:     - Read the file at PATH as a binary file,
        - Remove all null bytes (handle UTF-16, UCS-2, etc),
        - Split 'lines' on CR, CR+LF or LF,
        - Replace all bytes less than 32 or greater than 126 by #\?,
        - Convert the remaining bytes as ASCII codes into the CL standard set.
RETURN: The contents of the file as a list of base-string lines.
"
  (loop
     :with data   = (delete 0 (binary-file-contents
                               path :if-does-not-exist if-does-not-exist))
     :with cursor = (make-array 1 :element-type '(unsigned-byte 8)
                                :adjustable t
                                :displaced-to data :displaced-index-offset 0)
     :for bol = 0 :then eol
     :for eol = (or (position-if (lambda (ch) (or (= 10 ch) (= 13 ch)))
                                 data :start bol)
                    (length data))
     :collect (map 'string
                   (lambda (code)
                     (if (<= ASCII:SP code 126)
                         (aref ascii:*ASCII-CHARACTERS* (- code ASCII:SP))
                         #\?))
                    (adjust-array cursor (- eol bol)
                                        :displaced-to data
                                        :displaced-index-offset bol))
     :do (cond
           ((<= (length data) eol))
           ((and (= 13 (aref data eol))
                 (< (1+ eol) (length data))
                 (= 10 (aref data (1+ eol))))
            (incf eol 2))
           (t (incf eol)))
     :while (< eol (length data))))


(DEFUN REMOVE-FIRST-LINES (FILE-NAME LINE-COUNT &key (element-type 'character))
  "
DO:         Modifies the file at path FILE-NAME, 
            removing the LINE-COUNT first lines.
WARNING:    There's no backup: if the COPY-OVER fails, the file will be left
            in an unspecified state.
"
  (WITH-OPEN-FILE (FILE FILE-NAME :DIRECTION :IO 
                        :ELEMENT-TYPE element-type
                        :if-exists :overwrite
                        :IF-DOES-NOT-EXIST :ERROR)
    ;; skip over the LINE-COUNT first lines:
    (DOTIMES (I LINE-COUNT)
      (unless(print (READ-LINE FILE NIL NIL))
        (ERROR "Less than ~A lines in the file ~A." LINE-COUNT FILE-NAME)))
    ;; copy over the rest of the file to the start:
    (COPY-OVER FILE (FILE-POSITION FILE) 0)))


;;;; THE END ;;;;
