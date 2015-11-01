;;; # Thread-local storage
;;;
;;; The STM supports at most 63 concurrent threads.  The value is a bit
;;; arbitrary (in fact it's only chosen to help the bytelock implementation).
;;; In any case, such a value should be enough to saturate the cores on
;;; most machines these days, and can be bumped up a bit more without
;;; much work.
;;;
;;; The tradeoff is that a lot of things can be simplified when the number
;;; of threads is known ahead of time.  E.g., thread-local storage becomes
;;; a lookup in small vector.
;;;
;;; This is implemented by storing the thread id in *thread-id*, and
;;; everything else is then a load-time-value'ed global.  This is simpler
;;; (and smaller) than special variable access.  However, a lot of it
;;; is repetitive and can hoisted to the beginning of a function.
;;; WITH-TRANSACTION-DATA implements that.  That's still not enough for
;;; inline functions to exploit it (they're not affected by the lexical
;;; environment as modified by any macro), so an alternative is provided
;;; that expands with compiler macros (DEFINLINE-MACRO).
;;;
;;; The current thread id should be accessed with %thread-id%, which
;;; will exploit WITH-TRANSACTION-DATA. %actions%, %locks% and %transaction-id%
;;; are all implemented via this mechanism.

(in-package "STM")
(defconstant +thread-count+ 63)
(deftype thread-id ()
  `(mod ,+thread-count+))

;;; Thread local variables are all implemented via a single *thread-id*,
;;; of type (INTEGER 0 +thread-count+).  Everything else is looked up
;;; in global vectors.  The hooks are called before and after initialising
;;; each transaction.  This allows extensions to set local storage up.
(declaim (type thread-id *thread-id*))
(defvar *thread-id*)

(declaim (inline in-transaction-p))
(defun in-transaction-p ()
  (boundp '*thread-id*))

(define-symbol-macro %thread-id% (identity (truly-the thread-id *thread-id*)))

;; turn this into a byte spin lock?
(defglobal **thread-id-locks**
  (make-array +thread-count+ :initial-element 0))

(defglobal **backoff** 32)

(defun get-thread-id ()
  (when (boundp '*thread-id*)
    (return-from get-thread-id *thread-id*))
  (let ((locks **thread-id-locks**)
        (delay **backoff**))
    (loop
      (dotimes (i +thread-count+)
        (when (and (zerop (aref locks i))
                   (zerop (compare-and-swap (svref locks i)
                                            0 255)))
          (return-from get-thread-id i)))
      (when (> delay 256)
        (sb-unix:nanosleep 0 delay))
      (when (< delay (ash 1 20))
        (incf delay delay)))))

(defun release-thread-id (&optional (id %thread-id%))
  (setf (svref **thread-id-locks** id) 0)
  nil)

(declaim (type (simple-array t (#. (+ 6 (* 8 +thread-count+))))
               **thread-values**))
(defglobal **thread-values** (make-array (+ 6 (* 8 +thread-count+))))
;; TRULY-THE also incidentally blocks SETF
(define-symbol-macro %thread-values%
    (truly-the (simple-array t (#. (+ 6 (* 8 +thread-count+))))
               (opaque-identity (load-time-value **thread-values** t))))

(define-symbol-macro %seq-number%
    (the fixnum
      (aref %thread-values% (+ 6 (* 8 %thread-id%)))))
(define-symbol-macro %actions%
    (aref %thread-values%
          (+ 7 (* 8 %thread-id%))))
(define-symbol-macro %locks%
    (aref %thread-values%
          (+ 8 (* 8 %thread-id%))))
(define-symbol-macro %transaction-id%
    (the fixnum
      (aref %thread-values%
            (+ 9 (* 8 %thread-id%)))))

(defmacro define-thread-local-variable (name
                                     &key (type t)
                                          (initial-value nil initial-value-p))
  (let ((global (intern (format nil "*GLOBAL-VECTOR-FOR-~S*" name)))
        (array-type `(simple-array ,type (,+thread-count+))))
    `(progn
       (declaim (type ,array-type  ,global))
       (defglobal ,global (make-array +thread-count+
                                      :element-type ',type
                                      ,@(when initial-value-p
                                          `(:initial-element ,initial-value))))
       (define-symbol-macro ,name
           (aref (truly-the ,array-type
                            (opaque-identity (load-time-value ,global t)))
                 %thread-id%)))))

(defmacro with-transaction-data (&body body)
  (let ((id (gensym "ID"))
        (thread-values (gensym "THREAD-VALUES")))
    `(let ((,id %thread-id%)
           (,thread-values %thread-values%)) ;; make that extensible...
       (declare (type thread-id ,id)
                (type (simple-array t (#. (* 8 +thread-count+))))
                (ignorable ,id ,thread-values))
       (symbol-macrolet ((%thread-id% (identity ,id))
                         (%thread-values% (identity ,thread-values)))
         (flet ((in-transaction-p () t))
           (declare (ignorable #'in-transaction-p))
           ,@body)))))

(defmacro definline-macro (name lambda-list &body body)
  "Doing the inlining in a compiler macro allows us to frob the lexical
   environment and affect the expansion of the inlined function. Since
   it should only be used for optimisation purposes, a plain defun
   and an inline declaim should be equivalent."
  (let ((bare-ll (mapcar (lambda (x)
                           (if (atom x) x  (car x)))
                         lambda-list))
        (expr (gensym "EXPR")))
    `(progn
       (defun ,name ,lambda-list ,@body)
       (locally (declare (sb-ext:muffle-conditions style-warning))
         (define-compiler-macro ,name (&whole ,expr ,@bare-ll)
           `((lambda ,',lambda-list ,@',body)
             ,@(if (eql (first ,expr) 'funcall)
                   (cddr ,expr)
                   (cdr ,expr))))))))
