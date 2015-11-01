(defvar *alist-1* '((a . 1) (b . 2) (c . 3)))
(defvar *alist-2* '(("a" . 1) ("b" . 2) ("c" . 3)))
(defvar *alist-3* '((a . 10) (a . 1) (b . 2) (c . 3)))

(assoc 'a *alist-1*)
(cdr (assoc 'a *alist-1*))

(assoc "a" *alist-2* :test #'string=)
(assoc "a" *alist-2*)

(assoc 'a *alist-3*)

(acons 'd 4 *alist-3*)

(setf (getf *plist* :a) 1)
(setf (getf *plist* :b) 2)
(setf (getf *plist* :c) 3)

(get-properties *plist* '(:a :c))
(get-properties *plist* '(:a :b))
