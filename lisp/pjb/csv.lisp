;;;; -*- coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               csv.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This package reads and writes CSV files.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-09-01 <PJB> Made use of iso6429.
;;;;    2004-09-06 <PJB> Created.
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

(IN-PACKAGE "COMMON-LISP-USER")
(DECLAIM (DECLARATION ALSO-USE-PACKAGES)
         (ALSO-USE-PACKAGES "COM.INFORMATIMAGO.COMMON-LISP.ECMA048"))
(DEFPACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CSV"
  (:USE "COM.INFORMATIMAGO.COMMON-LISP.PEEK-STREAM" "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY")
  (:EXPORT "LOAD-RECORDS" "WRITE-RECORD")
  (:IMPORT-FROM "COM.INFORMATIMAGO.COMMON-LISP.UTILITY" "UNTIL" "WHILE")
  (:DOCUMENTATION
   "
    This package reads and writes CSV files.

    Copyright Pascal J. Bourguignon 2004 - 2005
   
    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version
    2 of the License, or (at your option) any later version.
    "))
(IN-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.CSV")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((*compile-verbose* nil)) (ecma048:generate-all-functions-in-ecma048)))


;; http://planet.plt-scheme.org/docs/neil/csv.plt/1/0/doc.txt


;; The CSV File Format
;; 
;;     * Each record is one line   ...but A record separator may consist
;;       of a line feed (ASCII/LF=0x0A), or a carriage return and line
;;       feed pair (ASCII/CRLF=0x0D 0x0A).  ...but: fields may contain
;;       embedded line-breaks (see below) so a record may span more than
;;       one line.
;; 
;;     * Fields are separated with commas.  Example John,Doe,120 any
;;       st.,"Anytown, WW",08123
;; 
;;     * Leading and trailing space-characters adjacent to comma field
;;       separators are ignored.  So   John  ,   Doe  ,... resolves to
;;       "John" and "Doe", etc. Space characters can be spaces, or tabs.
;; 
;;     * Fields with embedded commas must be delimited with double-quote
;;       characters.  In the above example. "Anytown, WW" had to be
;;       delimited in double quotes because it had an embedded comma.
;; 
;;     * Fields that contain double quote characters must be surounded by
;;       double-quotes, and the embedded double-quotes must each be
;;       represented by a pair of consecutive double quotes.  So, John
;;       "Da Man" Doe would convert to "John ""Da Man""",Doe, 120 any
;;       st.,...
;; 
;;     * A field that contains embedded line-breaks must be surounded by
;;       double-quotes
;;       So:
;;         Field 1: Conference room 1  
;;         Field 2:
;;           John,
;;           Please bring the M. Mathers file for review  
;;           -J.L.
;;         Field 3: 10/18/2002
;;         ...
;; 
;;       would convert to:
;;         Conference room 1, "John,  
;;         Please bring the M. Mathers file for review  
;;         -J.L.
;;         ",10/18/2002,...
;; 
;;       Note that this is a single CSV record, even though it takes up
;;       more than one line in the CSV file. This works because the line
;;       breaks are embedded inside the double quotes of the field.
;; 
;;     * Fields with leading or trailing spaces must be delimited with
;;       double-quote characters.  So to preserve the leading and
;;       trailing spaces around the last name above: John ,"   Doe   ",...
;; 
;;           o Usage note: Some applications will insist on helping you
;;             by removing leading and trailing spaces from all fields
;;             regardless of whether the CSV used quotes to preserve
;;             them. They may also insist on removing leading zeros from
;;             all fields regardless of whether you need them. One such
;;             application is Excel. :-(
;; 
;;     * Fields may always be delimited with double quotes.
;;       The delimiters will always be discarded.
;; 
;;           o Implementation note: When importing CSV, do not reach down
;;             a layer and try to use the quotes to impart type
;;             information to fields. Also, when exporting CSV, you may
;;             want to be defensive of apps that improperly try to do
;;             this. Though, to be honest, I have not found any examples
;;             of applications that try to do this. If you have
;;             encountered any apps that attempt to use the quotes to
;;             glean type information from CSV files (like assuming
;;             quoted fields are strings even if they are numeric),
;;             please let me know about it.
;; 
;;     * The first record in a CSV file may be a header record containing
;;       column (field) names There is no mechanism for automatically
;;       discerning if the first record is a header row, so in the
;;       general case, this will have to be provided by an outside
;;       process (such as prompting the user). The header row is encoded
;;       just like any other CSV record in accordance with the rules
;;       above. A header row for the multi-line example above, might be:
;;         Location, Notes, "Start Date", ...


;; field-names
;; 
;; " --> ""
;; " --> \"
;; " --> empty
;; 
;; ~% --> NL
;; ~% --> \n
;; ~% --> empty

(DEFGENERIC NEWLINE (SCANNER))
(DEFGENERIC SCAN-NEWLINE (SCANNER CH))
(DEFGENERIC GET-TOKEN (SCANNER))
(DEFGENERIC ADVANCE (PARSER))
(DEFGENERIC REPORT-ERROR (PARSER MESSAGE &REST ARGS))
(DEFGENERIC CSV-PARSE-FILE (SELF))
(DEFGENERIC CSV-PARSE-RECORD (SELF))


(DEFUN ESCAPE-FIELD (FIELD)
  (SETF FIELD (COND
                ((NULL FIELD) "")
                ((STRINGP FIELD) FIELD)
                (T  (FORMAT NIL "~A" FIELD))))
  (IF (POSITION (CHARACTER "\"") FIELD)
      (DO ((RESULT (MAKE-STRING (+ (LENGTH FIELD)
                                   (COUNT (CHARACTER "\"") FIELD))))
           (I 0 (1+ I))
           (J 0))
          ((>= I (LENGTH FIELD)) RESULT)
        (SETF (CHAR RESULT J) (CHAR FIELD I))
        (INCF J)
        (WHEN (CHAR= (CHARACTER "\"") (CHAR FIELD I))
          (SETF (CHAR RESULT J) (CHAR FIELD I))
          (INCF J)))
      FIELD)) ;;ESCAPE-FIELD


(DEFUN WRITE-RECORD (FIELDS &OPTIONAL (OUT *STANDARD-OUTPUT*))
  (LET ((*PRINT-PRETTY* NIL))
    (FORMAT OUT "~{\"~A\"~^,~}~%" (MAPCAR (FUNCTION ESCAPE-FIELD) FIELDS))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scanner
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; text   ::= { any-char-but-coma-new-line-and-dblquote } .
;; quoted-text ::= { any-char-but-coma-new-line-and-dblquote
;;                   | "\"\"" | "," | CR | LF } .
;; coma
;; new-line (cr, lf, cr-lf)


(DEFPARAMETER +C+CR+      (CODE-CHAR ecma048:cr))
(DEFPARAMETER +C+LF+      (CODE-CHAR ecma048:lf))
(DEFPARAMETER +NEWLINE+   (FORMAT NIL "~%"))
(DEFPARAMETER +CRLF+      (FORMAT NIL "~C~C" +C+CR+ +C+LF+))
(DEFPARAMETER +CR+        (FORMAT NIL "~C"   +C+CR+))
(DEFPARAMETER +LF+        (FORMAT NIL "~C"   +C+LF+))
(DEFPARAMETER +SPACES+    (FORMAT NIL " ~C" (CODE-CHAR ecma048:ht)))
(DEFPARAMETER +TEXT-TERM+ (FORMAT NIL ",~A" +CRLF+))


(DEFCLASS SCANNER ()
  ((SOURCE  :TYPE PEEK-STREAM
            :INITARG :SOURCE :ACCESSOR SCANNER-SOURCE)
   (NEWLINE :INITFORM NIL :TYPE SYMBOL :ACCESSOR SCANNER-NEWLINE)
   (CRCNT   :INITFORM 0 :TYPE INTEGER
            :ACCESSOR SCANNER-CRCNT)
   (LFCNT   :INITFORM 0 :TYPE INTEGER
            :ACCESSOR SCANNER-LFCNT)
   (CRLFCNT :INITFORM 0 :TYPE INTEGER
            :ACCESSOR SCANNER-CRLFCNT))
  (:DOCUMENTATION "A scanner.
If not set  newline is automatically determined from statistics on newlines
found in the source. At the beginning of the file, or when newlines are
inconsistent, a default newline = LF is used. This imports for \ escapes.")
  ) ;;SCANNER


(DEFUN MAKE-SCANNER (&KEY (SOURCE *STANDARD-INPUT*))
  (MAKE-INSTANCE 'SCANNER
    :SOURCE (MAKE-INSTANCE 'PEEK-STREAM :STREAM SOURCE)))


(DEFMETHOD PRINT-OBJECT ((SELF SCANNER) OUT)
  (FORMAT OUT "#<~A nl=~S cr=~D lf=~D crlf=~D source=~A>"
          (CLASS-NAME (CLASS-OF SELF))
          (SCANNER-NEWLINE SELF)
          (SCANNER-CRCNT SELF)
          (SCANNER-LFCNT SELF)
          (SCANNER-CRLFCNT SELF)
          (SCANNER-SOURCE SELF))
  SELF) ;;PRINT-OBJECT

          
(DEFMETHOD NEWLINE ((SCANNER SCANNER))
  "
RETURN:  The newline string determined by the heuristic:
         {crlfcnt,lfcnt} << crcnt ==> +cr+
         {crlfcnt,crcnt} << lfcnt ==> +lf+
         {crcnt,lfcnt} << crlfcnt ==> +crlf+
         otherwise                ==> +lf+
"
  (FLET ((<< (A B) (< (* 2 A) B))
         (SMALL (A) (<= A 2)))
    (MACROLET
        ((TRANS (NL KNL FNL K1 F1 K2 F2 &KEY WARN)
           `(CASE  (SCANNER-NEWLINE SCANNER)
              ((NIL)
               (SETF (SCANNER-NEWLINE SCANNER) :LF)
               +LF+)
              ((,KNL)
               ,(WHEN WARN
                      `(WARN "Newline is perhaps ~A. (~A=~D, ~A=~D, ~A=~D).~%"
                             ',KNL ,KNL (,FNL SCANNER)
                             ,K1 (,F1 SCANNER) ,K2 (,F2 SCANNER)))
               ,NL)
              (OTHERWISE
               (WARN "Newline changed from ~A to ~A (~A=~D, ~A=~D, ~A=~D).~%"
                     (SCANNER-NEWLINE SCANNER) ,KNL
                     ,KNL (,FNL SCANNER) ,K1 (,F1 SCANNER) ,K2 (,F2 SCANNER))
               (SETF (SCANNER-NEWLINE SCANNER) ,KNL)
               ,NL))))
      (COND
        ((AND (SMALL (SCANNER-CRCNT SCANNER))
              (SMALL (SCANNER-LFCNT SCANNER))
              (SMALL (SCANNER-CRLFCNT SCANNER)))  +LF+)
        ((AND (<< (SCANNER-CRCNT   SCANNER) (SCANNER-LFCNT SCANNER))
              (<< (SCANNER-CRLFCNT SCANNER) (SCANNER-LFCNT SCANNER)))
         (TRANS +LF+
                :LF   SCANNER-LFCNT   :CR SCANNER-CRCNT :CRLF SCANNER-CRLFCNT))
        ((AND (<< (SCANNER-LFCNT   SCANNER) (SCANNER-CRCNT SCANNER))
              (<< (SCANNER-CRLFCNT SCANNER) (SCANNER-CRCNT SCANNER)))
         (TRANS +CR+
                :CR   SCANNER-CRCNT   :LF SCANNER-LFCNT :CRLF SCANNER-CRLFCNT))
        ((AND (<< (SCANNER-LFCNT SCANNER) (SCANNER-CRLFCNT SCANNER))
              (<< (SCANNER-CRCNT SCANNER) (SCANNER-CRLFCNT SCANNER)))
         (TRANS +CRLF+
                :CRLF SCANNER-CRLFCNT :LF SCANNER-LFCNT :CR   SCANNER-CRCNT))
        ((AND (< (SCANNER-CRCNT   SCANNER) (SCANNER-LFCNT SCANNER))
              (< (SCANNER-CRLFCNT SCANNER) (SCANNER-LFCNT SCANNER)))
         (TRANS +LF+
                :LF   SCANNER-LFCNT   :CR SCANNER-CRCNT :CRLF SCANNER-CRLFCNT
                :WARN T))
        ((AND (< (SCANNER-LFCNT   SCANNER) (SCANNER-CRCNT SCANNER))
              (< (SCANNER-CRLFCNT SCANNER) (SCANNER-CRCNT SCANNER)))
         (TRANS +CR+
                :CR   SCANNER-CRCNT   :LF SCANNER-LFCNT :CRLF SCANNER-CRLFCNT
                :WARN T))
        ((AND (< (SCANNER-LFCNT SCANNER) (SCANNER-CRLFCNT SCANNER))
              (< (SCANNER-CRCNT SCANNER) (SCANNER-CRLFCNT SCANNER)))
         (TRANS +CRLF+
                :CRLF SCANNER-CRLFCNT :LF SCANNER-LFCNT :CR   SCANNER-CRCNT
                :WARN T))
        (T
         (WARN "Newline is completely random! (~A=~D, ~A=~D, ~A=~D).~%"
               :LF (SCANNER-LFCNT SCANNER)
               :CR (SCANNER-CRCNT SCANNER)
               :CRLF (SCANNER-CRLFCNT SCANNER))
         +LF+))))) ;;NEWLINE


(DEFMETHOD SCAN-NEWLINE ((SCANNER SCANNER) (CH CHARACTER))
  (COND
    ((CHAR= +C+CR+ CH)
     (IF (CHAR= +C+LF+ (NEXTCHAR (SCANNER-SOURCE SCANNER)))
         (PROGN
           (GETCHAR (SCANNER-SOURCE SCANNER))
           (INCF (SCANNER-CRLFCNT SCANNER)))
         (INCF (SCANNER-CRCNT SCANNER)))
     T)
    ((CHAR= +C+LF+ CH)
     (INCF (SCANNER-LFCNT SCANNER))
     T)
    (T
     NIL))) ;;SCAN-NEWLINE
 
       
(DEFMETHOD GET-TOKEN ((SCANNER SCANNER))
  "
NOTE:  Multiline values are returned as a list of lines.
BUG:   Line termination should be determined once for the whole file.
       '\' cr lf could mean cr, end of line, or newline
"
  (MACROLET
      ((GETCH   () `(GETCHAR  (SCANNER-SOURCE SCANNER)))
       (NEXTCH  () `(NEXTCHAR (SCANNER-SOURCE SCANNER)))
       (EAT-ESCAPE
           (CH VALUE)
         `(IF (NEXTCH)
              (PROGN
                (SETF ,CH (GETCH))
                (IF (CHAR= +C+CR+ ,CH)
                    (IF (EQ +CRLF+ (NEWLINE SCANNER))
                        (IF (CHAR= +C+LF+ (NEXTCH))
                            (PROGN
                              (VECTOR-PUSH-EXTEND +C+CR+  ,VALUE)
                              (VECTOR-PUSH-EXTEND (GETCH) ,VALUE))
                            (VECTOR-PUSH-EXTEND +C+CR+ ,VALUE))
                        (VECTOR-PUSH-EXTEND +C+CR+ ,VALUE))
                    (VECTOR-PUSH-EXTEND ,CH ,VALUE)))
              (ERROR "Found a '\\' at end of file."))))
    (LET ((CH (GETCH)))
      (WHILE (AND CH (POSITION CH +SPACES+) (SETF CH (GETCH))))
      (COND
        ((NULL CH) (VALUES :EOF NIL))
        ((CHAR= CH (CHARACTER ","))  (VALUES :COMA ","))
        ((CHAR= CH (CHARACTER "\""))
         ;; quoted-text ::= { any-char-but-coma-new-line-and-dblquote
         ;;                   | "\"\"" | "," | CR | LF } .
         (LET ((LINES '())
               (VALUE (MAKE-ARRAY '(16) :FILL-POINTER 0 :ADJUSTABLE T 
                                  :ELEMENT-TYPE 'CHARACTER)))
           (DO* ((CH (GETCH)    (OR EOS (GETCH)))
                 (EOS (NULL CH) (OR EOS (NULL CH))))
                (EOS
                 (IF LINES
                     (PROGN (PUSH VALUE LINES)                        
                            (VALUES :QUOTED-TEXT (NREVERSE LINES)))
                     (VALUES :QUOTED-TEXT VALUE)))
             (COND
               ((CHAR= CH (CHARACTER "\""))
                (IF (CHAR= CH (NEXTCH))
                    (PROGN (VECTOR-PUSH-EXTEND CH VALUE)
                           (GETCH))
                    (SETF EOS T)))
               ((CHAR= CH (CHARACTER "\\"))
                (EAT-ESCAPE CH VALUE))
               ((SCAN-NEWLINE SCANNER CH)
                (PUSH VALUE LINES)
                (SETF VALUE (MAKE-ARRAY '(16) :FILL-POINTER 0 :ADJUSTABLE T 
                                        :ELEMENT-TYPE 'CHARACTER)))
               (T (VECTOR-PUSH-EXTEND CH VALUE))))))
        ((SCAN-NEWLINE SCANNER CH)
         (VALUES :NEWLINE (NEWLINE SCANNER)))
        (T
         ;; text   ::= { any-char-but-coma-new-line-and-dblquote } .
         (LET ((VALUE (MAKE-ARRAY '(16) :FILL-POINTER 0 :ADJUSTABLE T 
                                  :ELEMENT-TYPE 'CHARACTER)))
           (UNTIL (OR (NULL CH) (POSITION CH +TEXT-TERM+))
             (IF (CHAR= CH (CHARACTER "\\"))
                 (EAT-ESCAPE CH VALUE)
                 (VECTOR-PUSH-EXTEND CH VALUE))
             (SETF CH (GETCH)))
           (WHEN CH (UNGETCHAR (SCANNER-SOURCE SCANNER) CH))
           (VALUES :TEXT (STRING-TRIM +SPACES+ VALUE)))))))) ;;GET-TOKEN

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFCLASS PARSER ()
  ((SCANNER     :ACCESSOR PARSER-SCANNER     :INITFORM NIL :INITARG :SCANNER)
   (TOKEN       :ACCESSOR PARSER-TOKEN       :INITFORM NIL)
   (VALUE       :ACCESSOR PARSER-VALUE       :INITFORM NIL)
   (NEXT-TOKEN  :ACCESSOR PARSER-NEXT-TOKEN  :INITFORM NIL)
   (NEXT-VALUE  :ACCESSOR PARSER-NEXT-VALUE  :INITFORM NIL))
  (:DOCUMENTATION "A parser.")) ;;PARSER


(DEFMETHOD PRINT-OBJECT ((SELF PARSER) OUT)
  (FORMAT OUT "#<~A :scanner ~S :token (~S ~S) :next (~S ~S)>"
          (CLASS-NAME (CLASS-OF SELF)) (PARSER-SCANNER SELF)
          (PARSER-TOKEN SELF)      (PARSER-VALUE SELF)
          (PARSER-NEXT-TOKEN SELF) (PARSER-NEXT-VALUE SELF))
  SELF) ;;PRINT-OBJECT

          
(DEFMETHOD ADVANCE ((PARSER PARSER))
  (MULTIPLE-VALUE-BIND (TOK VAL) (GET-TOKEN (PARSER-SCANNER PARSER))
    (SETF (PARSER-TOKEN PARSER)      (PARSER-NEXT-TOKEN PARSER)
          (PARSER-VALUE PARSER)      (PARSER-NEXT-VALUE PARSER) 
          (PARSER-NEXT-TOKEN PARSER) TOK
          (PARSER-NEXT-VALUE PARSER) VAL))
  PARSER) ;;ADVANCE


(DEFMETHOD REPORT-ERROR ((PARSER PARSER) MESSAGE &REST ARGUMENTS)
  (ERROR "~A; (~S ~S) (~S ~S)" (APPLY (FUNCTION FORMAT) NIL MESSAGE ARGUMENTS)
         (PARSER-TOKEN PARSER)
         (PARSER-VALUE PARSER)
         (PARSER-NEXT-TOKEN PARSER)
         (PARSER-NEXT-VALUE PARSER))) ;;REPORT-ERROR


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; csv-parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; file   ::= { record newline } .
;; record ::= field { "," field } .
;; field  ::= | data .
;; data   ::= text | "\"" quoted-text "\"" .


(DEFCLASS CSV-PARSER (PARSER) ())


(DEFMETHOD CSV-PARSE-FILE ((SELF CSV-PARSER))
  ;; file   ::= { record newline } .
  (WHILE (NULL (PARSER-TOKEN SELF)) (ADVANCE SELF))
  (LET ((RECORDS '()))
    (UNTIL (EQ :EOF (PARSER-TOKEN SELF))
      (LET ((RECORD (CSV-PARSE-RECORD SELF)))
        (IF (EQ :NEWLINE (PARSER-TOKEN SELF))
            (ADVANCE SELF)
            (IF (EQ :EOF (PARSER-TOKEN SELF))
                (REPORT-ERROR
                 SELF "Last record ends with end-of-file instead of end-of-line.")
                (REPORT-ERROR
                 SELF
                 "INTERNAL: csv-parse-record left a token than end-of-line.")))
        (WHEN RECORD (PUSH RECORD RECORDS))))
    (NREVERSE RECORDS))) ;;CSV-PARSE-FILE


(DEFMETHOD CSV-PARSE-RECORD ((SELF CSV-PARSER))
  ;; record ::= field { "," field } .
  ;; field  ::= | data .
  ;; data   ::= text | "\"" quoted-text "\"" .
  (LET ((FIELDS '()))
    (WHEN (EQ :COMA (PARSER-TOKEN SELF))
      (PUSH NIL FIELDS)
      (ADVANCE SELF))
    (WHILE (MEMBER (PARSER-TOKEN SELF) '(:QUOTED-TEXT :COMA :TEXT))
      (PUSH (IF (EQ :COMA (PARSER-TOKEN SELF)) NIL (PARSER-VALUE SELF))
            FIELDS)
      (ADVANCE SELF)
      (COND
        ((EQ :COMA (PARSER-TOKEN SELF))
         (ADVANCE SELF))
        ((MEMBER (PARSER-TOKEN SELF) '(:QUOTED-TEXT :TEXT))
         (REPORT-ERROR SELF "Missing a coma between two fields ~S and ~S."
                       (CAR FIELDS) (PARSER-VALUE SELF)))))
    (NREVERSE FIELDS))) ;;CSV-PARSE-RECORD


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reading CVS file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DEFUN LOAD-RECORDS (PATH
                     &KEY (EXTERNAL-FORMAT :DEFAULT) (ELEMENT-TYPE 'CHARACTER))
  (WITH-OPEN-FILE (INPUT PATH :DIRECTION :INPUT
                         :EXTERNAL-FORMAT EXTERNAL-FORMAT
                         :ELEMENT-TYPE ELEMENT-TYPE)
    (CSV-PARSE-FILE (MAKE-INSTANCE 'CSV-PARSER
                      :SCANNER (MAKE-SCANNER :SOURCE INPUT))))) ;;LOAD-RECORDS


;; Local Variables:
;; eval: (cl-indent 'while 1)
;; eval: (cl-indent 'until 1)
;; End:
      
;;;; csv.lisp                         --                     --          ;;;;

