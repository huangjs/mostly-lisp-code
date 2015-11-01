;;; benchmark utilities

(defpackage :benchmark
  (:use :cl)
  (:export #:benchmark))

(in-package :benchmark)

(defun average (results)
  (loop with times = (length results)
        for runtime in results
        sum runtime into total-runtime
        finally (return (float (/ total-runtime times) 1d0))))

(defun median (results)
  (setf results (sort (copy-list results) #'<))
  (elt results (floor (length results) 2)))

(defun standard-deviation (results)
  (loop with avg = (average results)
        with size = (length results)
        for runtime in results
        sum (expt (- runtime avg) 2) into sum-square-diff
        finally (return (sqrt (/ sum-square-diff size)))))

(defun benchmark% (func times verbose)
  (check-type func function)
  (assert (and (integerp times)
               (> times 0)))
  (let (begin results)
    (setf results
          (loop for i from 1 to times
                do (progn
                     (when verbose
                       (format t "~&Round: ~d, runtime: " i)
                       (finish-output))
                     (setf begin (get-internal-real-time))
                     (funcall func))
                collect
                (let ((runtime (- (get-internal-real-time) begin)))
                  (when verbose
                    (format t "~d ms~%" runtime)
                    (finish-output))
                  runtime)))
    (let ((median (median results))
          (standard-deviation (standard-deviation results)))
      (when verbose
        (format t "~&Average: ~6,fms~&Standard deviation: ~6,fms~&" median standard-deviation))
      (values (average results)
              results
              (list :median median
                    :standard-deviation standard-deviation)))))

(defmacro benchmark ((&key (times 1) verbose) &body body)
  `(benchmark% (lambda () ,@body) ,times ,verbose))


;;; from pkhuong's string-case.lisp
(defmacro with-timing ((total-iters subiters) &body forms)
  "More accurate timing (user time only), and returns (values min avg max) in secs"
  (let ((_thunk (gensym "THUNK"))
        (iters  (ceiling total-iters subiters)))
    `(flet ((,_thunk ()
              ,@forms))
       (let ((min sb-ext:double-float-positive-infinity)
             (sum 0d0)
             (max 0d0))
         (declare (type double-float min sum max))
         (loop repeat ,iters
               do (multiple-value-bind (_ begin/sec begin/us)
                      (sb-unix:unix-fast-getrusage sb-unix:rusage_self)
                    (declare (ignore _))
                    (loop repeat ,subiters
                          do (,_thunk))
                    (multiple-value-bind (_ end/sec end/us)
                        (sb-unix:unix-fast-getrusage sb-unix:rusage_self)
                      (declare (ignore _))
                      (let ((time (+ (float  (- end/sec begin/sec) 0d0)
                                     (* 1d-6 (- end/us begin/us)))))
                        (setf min (min time min)
                              sum (+   time sum)
                              max (max time max))
                        (values))))
               finally (return (values min
                                       (/ sum ,iters)
                                       max)))))))
