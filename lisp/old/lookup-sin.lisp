;;; Not good enough!
;;; use closure would be better.

;; This is where we cache all of the sine tables generated 
;; during compilation. The tables stay around at runtime 
;; so they can be used for lookups. 
(defvar *sin-tables* (make-hash-table)
  "A hash table of tables of sine values. The hash is keyed
by the number of entries in each sine table.")

;; This is a helper function for the LOOKUP-SIN macro. 
;; It is used only at compile time. 
(defun get-sin-table-and-increment (divisions)
  "Returns a sine lookup table and the number of radians quantized
by each entry in the table. Tables of a given size are reused.
A table covers angles from zero to pi/4 radians."
  (let ((table (gethash divisions *sin-tables* :none))
        (increment (/ pi 2 divisions)))
    (when (eq table :none)
      ;; Uncomment the next line to see when a table gets created. 
      ;;(print '|Making new table|) 
      (setq table
            (setf (gethash divisions *sin-tables*)
                  (make-array (1+ divisions) :initial-element 1.0)))
      (dotimes (i divisions)
        (setf (aref table i)
              (sin (* increment i)))))
    (values table increment)))

;; Macro calls the helper at compile time, and returns an 
;; AREF form to do the lookup at runtime. 
(defmacro lookup-sin (radians divisions)
  "Return a sine value via table lookup."
  (multiple-value-bind (table increment)
                       (get-sin-table-and-increment divisions)
    `(aref ,table (round ,radians ,increment))))

;; run
;; (pprint (macroexpand-1 '(lookup-sin (/ pi 4) 50)))
