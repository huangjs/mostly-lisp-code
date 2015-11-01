;;; printing date
(format nil "~4,'0d-~2,'0d-~2,'0d" 2005 6 10)


;;; print digitals using "." instead of ","
(format nil "~,,'.,4:d" 100000000)

;;; print lexical numbers
(format nil "~r" 1000000)

(format nil "~r files~:p" 1)
(format nil "~r files~:p" 10)
(format nil "~r files~:p" 0)

(format nil "~r famil~:@p" 1)
(format nil "~r famil~:@p" 100)
(format nil "~r famil~:@p" 0)

;;; Captalization
(format nil "~(~a~)" "tHe Quick BROWN foX") ;~ "the quick brown fox"
(format nil "~@(~a~)" "tHe Quick BROWN foX") ;~ "The quick brown fox"
(format nil "~:(~a~)" "tHe Quick BROWN foX") ;~ "The Quick Brown Fox"
(format nil "~:@(~a~)" "tHe Quick BROWN foX") ;~ "THE QUICK BROWN FOX"

;;; Conditional
(format nil "~[cero~;uno~;dos~]" 0) ;~ "cero"
(format nil "~[cero~;uno~;dos~]" 1) ;~ "uno"
(format nil "~[cero~;uno~;dos~]" 2) ;~ "dos"

(format nil "~[cero~;uno~;dos~:;mucho~]" 3) ~ "mucho"
(format nil "~[cero~;uno~;dos~:;mucho~]" 100) ~ "mucho"

(format t "~:[FAIL~;pass~]" nil) ;-> fail
(format t "~:[FAIL~;pass~]" t) ;-> pass
(format t "~:[FAIL~;pass~]" 0) ;-> pass

;;; Example
(defparameter *list-etc*
"~#[NONE~;~a~;~a and ~a~:;~a, ~a~]~#[~; and ~a~:;, ~a, etc~].")

(format nil *list-etc*) ;~ "NONE."
(format nil *list-etc* 'a) ;~ "A."
(format nil *list-etc* 'a 'b) ;~ "A and B."
(format nil *list-etc* 'a 'b 'c) ;~ "A, B and C."
(format nil *list-etc* 'a 'b 'c 'd) ;~ "A, B, C, etc."
(format nil *list-etc* 'a 'b 'c 'd 'e) ;~ "A, B, C, etc."

;;; Example
(format nil "~@[x = ~a ~]~@[y = ~a~]" 10 20) ;~ "x = 10 y = 20"
(format nil "~@[x = ~a ~]~@[y = ~a~]" 10 nil) ;~ "x = 10 "
(format nil "~@[x = ~a ~]~@[y = ~a~]" nil 20) ;~ "y = 20"
(format nil "~@[x = ~a ~]~@[y = ~a~]" nil nil) ;~ ""


;;; Iteration
(format nil "~{~a, ~}" (list 1 2 3)) ;~ "1, 2, 3, "
(format nil "~{~a~^, ~}" (list 1 2 3)) ;~ "1, 2, 3"

(format nil "~{~a~#[~;, and ~:;, ~]~}" (list 1 2 3)) ;~ "1, 2, and 3"

;;; Example3
; (defparameter *english-list*
; "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")

(defparameter *english-list*
"~{~#[<empty>~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~:}") ; a better format

(format nil *english-list* '()) ;~ ""
(format nil *english-list* '(1)) ;~ "1"
(format nil *english-list* '(1 2)) ;~ "1 and 2"
(format nil *english-list* '(1 2 3)) ;~ "1, 2, and 3"
(format nil *english-list* '(1 2 3 4)) ;~ "1, 2, 3, and 4"


;;; Hop, skip, jump
(format nil "I saw ~r el~:*~[ves~;f~:;ves~]." 0) ;~ "I saw zero elves."
(format nil "I saw ~r el~:*~[ves~;f~:;ves~]." 1) ;~ "I saw one elf."
(format nil "I saw ~r el~:*~[ves~;f~:;ves~]." 2) ;~ "I saw two elves."

(format nil "I saw ~[no~:;~:*~r~] el~:*~[ves~;f~:;ves~]." 0) ; a better format

;;; example 4
(format nil "~{~s~*~^ ~}" '(:a 10 :b 20)) ;~ ":A :B"

