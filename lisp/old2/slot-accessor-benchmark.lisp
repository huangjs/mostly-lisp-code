#+nil
(require :sb-sprof)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-symbol "EMIT-CACHE-LOOKUP" :sb-pcl)
    (pushnew :new-cache *features*)))

(defpackage :slot-value-access
  (:use :cl))

(in-package :slot-value-access)

(declaim (notinline foo))
(defun foo (x)
  x)

(defparameter *tests* nil)

;;; N classes, one slot, one accessor

(defmacro define-n-classes (n m)
  (let ((slot (sb-pcl::format-symbol *package* "~A-CLASS-SLOT" n))
        (test (sb-pcl::format-symbol *package* "~A-CLASS-RUN" n)))
    `(progn
       ,@(loop for i from 1 upto n
               collect `(defclass ,(sb-pcl::format-symbol *package*
                                                          "~A-CLASS.~A" n i)
                            ()
                          ((,slot
                            :initform (random most-positive-fixnum)
                            :accessor ,slot))))
       (defun ,test (instances k)
         (declare (fixnum k))
         (dotimes (i k)
           (dolist (elt instances)
             (foo (,slot elt))
             (setf (,slot elt) 42))))
       (push (lambda ()
               (let ((list (loop for i from 1 upto ,n
                                 collect
                                 (make-instance (sb-pcl::format-symbol
                                                 *package*
                                                 "~A-CLASS.~A" ,n i)))))
                 (lambda ()
                   (format *trace-output* "~&===~A===~%" ',slot)
                   (time (,test list 2))
                   (format *trace-output* "~& - after warm-up -~%")
                   (time (,test list ,(truncate m n))))))
             *tests*))))

(define-n-classes 1 1000000)
(define-n-classes 2 1000000)
(define-n-classes 8 1000000)
(define-n-classes 32 1000000)
(define-n-classes 128 1000000)
(define-n-classes 512 1000000)

;;; N classes, 2 slot locations, 1 accessor

(defmacro define-n-classes/2-slots (n m)
  (let ((slot (sb-pcl::format-symbol *package* "~A-CLASS/2-SLOT-SLOT" n))
        (test (sb-pcl::format-symbol *package* "~A-CLASS/2-SLOT-RUN" n)))
    `(progn
       ,@(loop for i from 1 upto n
               collect `(defclass ,(sb-pcl::format-symbol
                                    *package*
                                    "~A-CLASS/2-SLOT.~A" n i)
                            ()
                          ,(if (oddp i)
                               `((,slot
                                  :initform (random most-positive-fixnum)
                                  :accessor ,slot)
                                 (dummy
                                  :initform (random most-positive-fixnum)))
                               `((dummy
                                  :initform (random most-positive-fixnum))
                                 (,slot
                                  :initform (random most-positive-fixnum)
                                  :accessor ,slot)))))
       (defun ,test (instances k)
         (declare (fixnum k))
         (dotimes (i k)
           (dolist (elt instances)
             (foo (,slot elt))
             (setf (,slot elt) 42))))
       (push (lambda ()
               (let ((list (loop for i from 1 upto ,n
                                 collect
                                 (make-instance
                                  (sb-pcl::format-symbol *package*
                                                          "~A-CLASS/2-SLOT.~A"
                                                          ,n i)))))
                 (lambda ()
                   (format *trace-output* "~&===~A===~%" ',slot)
                   (time (,test list 2))
                   (format *trace-output* "~& - after warm-up - ~%")
                   (time (,test list ,(truncate m n))))))
             *tests*))))

(define-n-classes/2-slots 2 1000000)
(define-n-classes/2-slots 8 1000000)
(define-n-classes/2-slots 32 1000000)
(define-n-classes/2-slots 128 1000000)
(define-n-classes/2-slots 512 1000000)

(format *trace-output* "~A ~A, SLOT-VALUE-ACCESS~%"
        (lisp-implementation-type)
        (lisp-implementation-version))

(defvar *stats*)

(let ((test-lambdas (nreverse (mapcar #'funcall *tests*))))
  #+nil
  (sb-sprof:start-profiling :sample-interval 0.0001)
  #+new-cache-stats  
  (sb-pcl::record-cache-stats)
  (dolist (f test-lambdas)
    (funcall f))
  #+new-cache-stats
  (setf *stats* (sb-pcl::clear-cache-stats)))

#+nil
(progn
  (sb-sprof:stop-profiling)
  (sb-sprof:report :type :flat))

(do-symbols (s *package*)
  (when (and (eq *package* (symbol-package s))
             (fboundp s)
             (typep (fdefinition s) 'standard-generic-function))
    (let ((cache (second (sb-pcl::gf-dfun-state (fdefinition s)))))
      (when (typep cache 'sb-pcl::cache)
        #+new-cache
        (format *trace-output*
                "~S depth: ~A, waste: ~,2F%~%"
                s
                (sb-pcl::cache-max-depth cache)
                (let* ((vector (sb-pcl::cache-vector cache))
                       (used (count-if #'identity vector))
                       (size (length vector)))
                  (* 100.0 (/ (- size used) size))))
        #-new-cache
        (format *trace-output*
                "~S waste: ~,2F%~%"
                s
                (let* ((vector (sb-pcl::cache-vector cache))
                       (used (count-if #'identity vector))
                       (size (length vector)))
                  (* 100.0 (/ (- size used) size))))))))

#+new-cache-stats
(progn
  (let* ((hits (car *stats*))
         (misses (cdr *stats*))
         (hit-count (length hits))
         (miss-count (length misses))
         (sum (+ hit-count miss-count)))
    (format *trace-output* "~&~D cache-reads, ~,2F% hits, ~,2F% misses~%"
            sum
            (* 100.0 (/ hit-count sum))
            (* 100.0 (/ miss-count sum))))
  
  (let* ((hits (remove :probe (car *stats*) :key #'cdr))
         (misses (remove :probe (cdr *stats*) :key #'cdr))
         (hit-count (length hits))
         (miss-count (length misses))
         (sum (+ hit-count miss-count)))
    (format *trace-output* "~&~D emitted lookups, ~,2F% hits, ~,2F% misses~%"
            sum
            (* 100.0 (/ hit-count sum))
            (* 100.0 (/ miss-count sum))))
  
  (let* ((hits (remove :lookup (car *stats*) :key #'cdr))
         (misses (remove :lookup (cdr *stats*) :key #'cdr))
         (hit-count (length hits))
         (miss-count (length misses))
         (sum (+ hit-count miss-count)))
    (format *trace-output* "~&~D probe-cache calls, ~,2F% hits, ~,2F% misses~%"
            sum
            (* 100.0 (/ hit-count sum))
            (* 100.0 (/ miss-count sum)))))
