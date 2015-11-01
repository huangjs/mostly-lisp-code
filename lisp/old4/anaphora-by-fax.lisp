(defpackage :anaphora
  (:use :cl)
  (:export :it
           :aif :acond :awhen
           :aand :aor :aprogn
           :acase :accase :aecase
           :atypecase :actypecase :aetypecase))
(in-package :anaphora)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun it-variable-p (symbol)
    "Semipredicate, is the symbol and it variable? if so return its canonicalized form"
    (let* ((name (symbol-name symbol))
           (length (length name)))
      (when (and (>= length 4)
                 (string= (subseq name (- length 4)) "->IT"))
        (intern (subseq name 0 (- length 4))))))

  (defun dejunk-it-variable (symbol) (or (it-variable-p symbol) symbol))

  (defun parse-anaphoric-lambda-list (lambda-list &aux (length (length lambda-list)))
    "Returns (values reqvars optvars restvars bodyvars) at least one of restvars or bodyvars will be nil"
    (let ((optional-place (position '&optional lambda-list))
          (rest-place     (position '&rest lambda-list))
          (body-place     (position '&body lambda-list)))
      (assert (not (and rest-place body-place)))
      (values (subseq lambda-list 0 (min (or optional-place length)
                                         (or rest-place length)
                                         (or body-place length)))
              (when optional-place (subseq lambda-list (1+ optional-place)
                                           (min (or rest-place length)
                                                (or body-place length))))
              (when rest-place (subseq lambda-list (1+ rest-place)))
              (when body-place (subseq lambda-list (1+ body-place))))))

  (defun canonicalize-anaphoric-lambda-list (lambda-list)
    (multiple-value-bind (reqvars optvars restvars bodyvars)
        (parse-anaphoric-lambda-list lambda-list)
      (append (mapcar #'dejunk-it-variable reqvars)
              (when optvars '(&optional))
              (mapcar #'dejunk-it-variable optvars)
              (when restvars '(&rest))
              (when bodyvars '(&body))
              (let ((rest (first (or restvars bodyvars))))
                (etypecase rest
                  (null)
                  (cons (list (dejunk-it-variable (first rest))))
                  (symbol (list (dejunk-it-variable rest))))))))

  (defun make-it-set-form (maybe-it-variable)
    (if (it-variable-p maybe-it-variable)
        `(list 'setf 'it ,(dejunk-it-variable maybe-it-variable))
        maybe-it-variable))

  (defun make-it-set-mapcar-form (maybe-it-variable)
    (if (it-variable-p maybe-it-variable)
        `(mapcar #'(lambda (form) `(setf it ,form))
                 ,(dejunk-it-variable maybe-it-variable))
        maybe-it-variable))

  (defun make-it-set-destructuring-bind-form (form)
    `(mapcar #'(lambda (elt)
                 (destructuring-bind (,@(canonicalize-anaphoric-lambda-list form)) elt
                   ,(cons 'list* (generate-anaphoric-macro-set-forms form))))
             ,(dejunk-it-variable (first form))))

  (defun generate-anaphoric-macro-set-forms (anaphoric-lambda-list)
    (multiple-value-bind (reqvars optvars restvars bodyvars)
        (parse-anaphoric-lambda-list anaphoric-lambda-list)
      (append
       (mapcar #'make-it-set-form reqvars)
       (mapcar #'make-it-set-form optvars)
       (list
        (let ((rest (first (or restvars bodyvars))))
          (etypecase rest
            (null)
            (cons (make-it-set-destructuring-bind-form rest))
            (symbol (make-it-set-mapcar-form rest))))))))

  (defun generate-anaphoric-macro-body (anaphoric-macro-specification)
    (append (list 'list*)
            (list `',(first anaphoric-macro-specification))
            (generate-anaphoric-macro-set-forms (rest anaphoric-macro-specification))))

  (defmacro define-anaphoric-macro (anaphoric-macro-specification)
    (let ((anaphoric-macro-name
           (intern (concatenate 'string "A" (symbol-name (first anaphoric-macro-specification)))))
          (canonicalized-lambda-list
           (canonicalize-anaphoric-lambda-list (rest anaphoric-macro-specification)))
          (macro-body
           (generate-anaphoric-macro-body anaphoric-macro-specification)))
      `(defmacro ,anaphoric-macro-name (,@canonicalized-lambda-list)
         (list 'let '(it)
               ,macro-body)))))

(define-anaphoric-macro (if test->it then &optional else))
(define-anaphoric-macro (cond &rest (clauses->it &rest consequence)))
(define-anaphoric-macro (when test->it &body forms))

(define-anaphoric-macro (and &rest forms->it))
(define-anaphoric-macro (or &rest forms->it))
(define-anaphoric-macro (progn &rest forms->it))

(define-anaphoric-macro (case keyform->it &body cases))
(define-anaphoric-macro (ccase keyform->it &body cases))
(define-anaphoric-macro (ecase keyform->it &body cases))

(define-anaphoric-macro (typecase keyform->it &body cases))
(define-anaphoric-macro (ctypecase keyform->it &body cases))
(define-anaphoric-macro (etypecase keyform->it &body cases))

