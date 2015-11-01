(defvar *foo-table* (make-hash-table))

(defun find-foo-cell (name create)
  (or (gethash name *foo-table*)
      (when create
        (setf (gethash name *foo-table*)
              (cons name nil)))))

(defun foo-from-cell (cell errorp &optional name)
  (or (cdr cell)
      (when errorp
        (error "No FOO called ~S." (or (car cell) name)))))

(defun find-foo (name &optional errorp)
  (foo-from-cell (find-foo-cell name nil) errorp name))

(define-compiler-macro find-foo (&whole form name &optional errorp)
  (if (constantp name)
      `(foo-from-cell (load-time-value (find-foo-cell ,name t)) ,errorp)
      form))

(defun (setf find-foo) (foo name &optional errorp)
  (declare (ignore errorp))
  (setf (cdr (find-foo-cell name t)) foo))

(define-compiler-macro (setf find-foo)
    (&whole form value name &optional errorp)
  (declare (ignore errorp))
  (if (constantp name)
      `(setf (cdr (load-time-value (find-foo-cell ,name t))) ,value)
      form))

;; Depending on your implementation's support for SETF-compiler-macros, you
;; may need to replace the SETF-function with

;; (defsetf find-foo set-foo) ; then SET-FOO and a compiler-macro for it

