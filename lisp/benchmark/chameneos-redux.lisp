;; The Computer Language Benchmarks Game
;;   http://shootout.alioth.debian.org/
;;
;;   contributed by Alexey Voznyuk
;;

(defpackage #:smp-utils
  (:use :cl :sb-alien :sb-thread)
  (:export #:affinity #:apic-core-map))

(in-package :smp-utils)

(defun cpuset->list (cpuset)
  (loop :for i :from 0 :below 128
    :unless (zerop (ldb (byte 1 (mod i 8)) (elt cpuset (truncate i 8))))
    :collect i))

(defun list->cpuset (cpuset-list)
  (loop :with cpuset = (make-array 16 :element-type '(unsigned-byte 8))
    :for i :from 0 :below 128
    :when (find i cpuset-list :test #'=)
    :do (setf (ldb (byte 1 (mod i 8)) (elt cpuset (truncate i 8))) 1)
    :finally (return cpuset)))

(defun affinity (thread)
  (with-alien ((alien-cpuset (array unsigned-char 16)))
    (let ((retcode (alien-funcall (extern-alien "pthread_getaffinity_np"
                                                (function int
                                                          unsigned-long
                                                          unsigned-long
                                                          (* unsigned-char)))
                                  (sb-thread::thread-os-thread thread)
                                  16
                                  (cast alien-cpuset (* unsigned-char)))))
      (when (zerop retcode)
        (values t (loop :with cpuset = (make-array 16 :element-type '(unsigned-byte 8))
                    :for i :from 0 :below 16
                    :do (setf (elt cpuset i) (deref alien-cpuset i))
                    :finally (return (cpuset->list cpuset))))))))

(defun (setf affinity) (affinity thread)
  (with-alien ((alien-cpuset (array unsigned-char 16)))
    (loop :with cpuset = (list->cpuset affinity)
      :for i :from 0 :below 16
      :do (setf (deref alien-cpuset i) (elt cpuset i)))
    (zerop (alien-funcall (extern-alien "pthread_setaffinity_np"
                                        (function int
                                                  unsigned-long
                                                  unsigned-long
                                                  (* unsigned-char)))
                          (sb-thread::thread-os-thread thread)
                          16
                          (cast alien-cpuset (* unsigned-char))))))

(defun apic-core-map (cpuset-list)
  (let ((default-map (mapcar #'list cpuset-list cpuset-list)))
    (unless (probe-file #p"/proc/cpuinfo")
      (return-from apic-core-map default-map))
    (with-open-file (cpuinfo #p"/proc/cpuinfo")
      (flet ((parse-key-value (line key)
               (when (and (> (length line) (length key))
                          (string= line key :end1 (length key)))
                 (let ((value-offset (position #\: line :start (length key))))
                   (when value-offset
                     (parse-integer line :start (1+ value-offset) :junk-allowed t))))))
        (loop :with current-cpu = nil
          :for line = (read-line cpuinfo nil nil)
          :while line
          :do (multiple-value-bind (processor apicid)
                  (values (parse-key-value line "processor")
                          (parse-key-value line "apicid"))
                (cond ((and current-cpu apicid) (setf (first (find current-cpu default-map :key #'second)) apicid
                                                      current-cpu nil))
                      (processor (setf current-cpu processor))))
          :finally (return (sort default-map #'< :key #'first)))))))


(defpackage #:chameneos-redux
  (:use :cl :smp-utils))

(in-package :chameneos-redux)

;;
;; Game DSL compiler
;;

(defmacro declare-colors-map (&rest transformations)
  `(progn
     (defun complement-color (color-a color-b)
       (cond
         ,@(loop
             :for (test-a kw-plus test-b kw-arrow test-result) :in transformations
             :do (assert (and (eq kw-plus '+) (eq kw-arrow '->)))
             :collect `((and (eq color-a ',test-a) (eq color-b ',test-b))
                        ',test-result))
         (t (error "Invalid colors combinations"))))
     (defun print-colors ()
       (format t "~{~{~a + ~a -> ~a~%~}~}~%"
               (list ,@(loop
                         :for (test-a kw-plus test-b) :in transformations
                         :collect `(list ,(string-downcase (string test-a))
                                         ,(string-downcase (string test-b))
                                         (string-downcase
                                          (string (complement-color ',test-a
                                                                    ',test-b))))))))))

(defun spell-number (number)
  (with-output-to-string (result-string)
    (loop
      :for char :across (the simple-string (format nil "~a" number))
      :do (format result-string " ~r" (- (char-code char) (char-code #\0))))))

(defmacro spin-wait (condition &key no-spin)
  (let ((yield-spin `(loop :until ,condition :do (sb-thread:thread-yield))))
    (if no-spin
        yield-spin
        `(loop
           :repeat 16384
           :do (when ,condition
                 (return))
           :finally ,yield-spin))))

(defstruct chameneo
  (color 'none :type symbol)
  (meet-count 0 :type fixnum)
  (same-count 0 :type fixnum)
  (meet-wait nil :type boolean))

(defmacro with-games ((&rest descriptions) &body body)
  (if (null descriptions)
      `(progn ,@body)
      (destructuring-bind (game-name &rest colors)
          (car descriptions)
        (let* ((colors-count (length colors))
               (worker-binds (loop :repeat colors-count :collect (gensym)))
               (chameneos (gensym "CHAMENEOS"))
               (action-cas (gensym "ACTION-CAS")))
          `(let ((,chameneos (coerce (list ,@(loop :repeat colors-count :collect `(make-chameneo)))
                                     'simple-vector))
                 (,action-cas (list 0))
                 ,@worker-binds)
             (declare (type (simple-vector ,colors-count) ,chameneos)
                      (type cons ,action-cas)
                      (type (or null sb-thread:thread) ,@worker-binds))
             (flet ((,(intern (format nil "RUN-~a" game-name)) (count threads-affinity smp-p)
                      (declare (type fixnum count) (type list threads-affinity) (type boolean smp-p))
                      (setf (car ,action-cas) (the fixnum (ash count ,(integer-length (1+ colors-count)))))
                      (flet ((color-worker (id color)
                               (declare (type (integer 0 ,(1- colors-count)) id) (type symbol color))
                               (lambda ()
                                 (setf (affinity sb-thread:*current-thread*) threads-affinity)
                                 (let ((state (car ,action-cas))
                                       (self (elt ,chameneos id)))
                                   (declare (type (integer 0 ,most-positive-fixnum) state)
                                            (type chameneo self))
                                   (setf (chameneo-color self) color)
                                   (loop
                                     (when (zerop state)
                                       (return))
                                     (let* ((peer-id (logand state ,(1- (ash 1 (integer-length (1+ colors-count))))))
                                            (new-state (if (zerop peer-id)
                                                           (logior state (1+ id))
                                                           (- state peer-id ,(ash 1 (integer-length (1+ colors-count)))))))
                                       (declare (type (integer 0 ,(1+ colors-count)) peer-id)
                                                (type (integer 0 ,most-positive-fixnum) new-state))
                                       (let ((prev-state (sb-ext:compare-and-swap (car ,action-cas) state new-state)))
                                         (declare (type (integer 0 ,most-positive-fixnum) prev-state))
                                         (if (= prev-state state)
                                             (progn
                                               (if (zerop peer-id)
                                                   (progn
                                                     (if smp-p
                                                         (spin-wait (chameneo-meet-wait self))
                                                         (spin-wait (chameneo-meet-wait self) :no-spin t))
                                                     (setf (chameneo-meet-wait self) nil))
                                                   (let ((peer (elt ,chameneos (1- peer-id))))
                                                     (when (= id (1- peer-id))
                                                       (incf (chameneo-same-count self))
                                                       (incf (chameneo-same-count peer)))
                                                     (let ((new-color (complement-color (chameneo-color self)
                                                                                        (chameneo-color peer))))
                                                       (declare (type symbol new-color))
                                                       (setf (chameneo-color self) new-color
                                                             (chameneo-color peer) new-color)
                                                       (incf (chameneo-meet-count self))
                                                       (incf (chameneo-meet-count peer))
                                                       (setf (chameneo-meet-wait peer) t))))
                                               (setf state (car ,action-cas)))
                                             (setf state prev-state)))))))))
                        ,@(loop :for color :in colors :for thread-index :from 0
                            :collect `(setf ,(elt worker-binds thread-index)
                                            (sb-thread:make-thread (color-worker ,thread-index ',color)
                                                                   :name ,(format nil "chameneos-worker-~a-~a/~a"
                                                                                  (string-downcase (string color))
                                                                                  thread-index
                                                                                  colors-count)))))
                      nil)
                    (,(intern (format nil "WAIT-~a" game-name)) ()
                      ,@(loop :for i :from 0 :below colors-count :collect `(sb-thread:join-thread ,(elt worker-binds i)))
                      (format t ,(format nil "~{ ~a~}~~%" (loop :for color :in colors :collect (string-downcase (string color)))))
                      (loop :for i :from 0 :below ,colors-count
                        :summing (chameneo-meet-count (elt ,chameneos i)) :into total :of-type fixnum
                        :do (format t "~a~a~%"
                                    (chameneo-meet-count (elt ,chameneos i))
                                    (spell-number (chameneo-same-count (elt ,chameneos i))))
                        :finally (format t "~a~%~%" (spell-number total)))))
               (with-games (,@(cdr descriptions))
                 ,@body)))))))


;;
;; Game contents
;;

(progn
  (declare-colors-map
   (blue + blue -> blue)
   (blue + red -> yellow)
   (blue + yellow -> red)
   (red + blue -> yellow)
   (red + red -> red)
   (red + yellow -> blue)
   (yellow + blue -> red)
   (yellow + red -> blue)
   (yellow + yellow -> yellow))

  (defun run-games (count current-affinity)
    (declare (optimize (speed 3) (safety 0) (debug 0))
             (type fixnum count)
             (type list current-affinity))
    (let* ((active-cores (length current-affinity))
           (smp-p (> active-cores 1)))
      (with-games ((game-a blue red yellow)
                   (game-b blue red yellow red yellow blue red yellow red blue))
        (if smp-p
            (multiple-value-bind (affinity-a affinity-b)
                (if (< active-cores 4)
                    (values current-affinity current-affinity)
                    (let ((apic-map (apic-core-map current-affinity)))
                      (declare (type list apic-map))
                      (values (list (second (elt apic-map 0)) (second (elt apic-map 1)))
                              (list (second (elt apic-map 2)) (second (elt apic-map 3))))))
              (run-game-a count affinity-a smp-p)
              (run-game-b count affinity-b smp-p)
              (wait-game-a)
              (wait-game-b))
            (progn (run-game-a count current-affinity smp-p)
                   (wait-game-a)
                   (run-game-b count current-affinity smp-p)
                   (wait-game-b))))))

  (defun main (&optional force-count)
    (let* ((args (cdr sb-ext:*posix-argv*))
           (count (or force-count (if args (parse-integer (car args)) 600))))
      (print-colors)
      (multiple-value-bind (success-p current-affinity)
          (affinity sb-thread:*current-thread*)
        (unless success-p
          (error "Failed to retrieve current thread affinity"))
        (run-games count current-affinity)))))


(in-package :cl-user)

(defun main ()
  (chameneos-redux::main))
