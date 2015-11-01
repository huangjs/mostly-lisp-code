;:*=======================
;:* constructor
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd)
  (push cd *db*))

;:*=======================
;:* just for example.
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))

;:*=======================
;:* display
(defun pretty-print-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

;:*=======================
;:* input
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: "))
          (return))))

;:*=======================
;:* save and load
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    ;;; use the standard format to print for reading back.
    (with-standard-io-syntax
        ;;; print prints lisp objects that can be read back in by the Lisp reader
        (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
        (setf *db* (read in)))))

(defun select (selector)
  (remove-if-not selector *db*))

(defun artist-selector (artist)
  #'(lambda (cd)
       (equal (getf cd :artist) artist)))

(defun title-selector (title)
  #'(lambda (cd)
      (equal (getf cd :title) title)))

(defun rating-selector (rating)
  #'(lambda (cd)
      (equal (getf cd :rating) rating)))

(defun select-by-artist (artist)
  (select (artist-selector artist)))

(defun select-by-title (title)
  (select (title-selector title)))

(defun select-by-rating (rating)
  (select (rating-selector rating)))

;:*=======================
;:* using keyword parameters
;;; like: (select (where :artist "Dixie Chicks"))
;;; or:   (select (where :rating 10 : ripped nil))
;:*=======================
;:* where is also a SELECTOR!!
; (defun where (&key title artist rating (ripped nil ripped-p))
;   #'(lambda (cd)
;       (and
;        (if title    (equal (getf cd :title) title) t)
;        (if artist   (equal (getf cd :artist) artist) t)
;        (if rating   (equal (getf cd :rating) rating) t)
;        (if ripped-p (equal (getf cd :ripped) ripped) t))))

(defun update (selector &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun delete-rows (selector)
  (setf *db* (remove-if selector *db*)))

;:*=======================
;:* using MACROS to eliminate duplication
(defmacro backwards (expr) (reverse expr))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparison-list (fields)
  (loop while fields
    collecting (make-comparison-expr (pop fields) (pop fields))))

;;; @ means flat-list
(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparison-list clauses))))

