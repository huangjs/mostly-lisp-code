(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro check (form)
  `(report-result ,form ',form))

;;; example
(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))

;;; progn can be replaced by AND which is a short circuiting.
;;; but short circuiting is not wanted, so below we will define a macro : combine-results
(defmacro check (&body forms)
  `(progn
    ,@(loop for f in forms collect `(report-result ,f ',f))))

;;; then
(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
  
;;; combine-results
(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
      ,result)))

;;; then check becomes
(defmacro check (&body forms)
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

;;; test-*
(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

;;; combine both
(defun test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

;;; with callee's information
;;; NOTE: only half abstractions, too bad!
(defvar *test-name* nil)

(defun test-+ ()
  (let ((*test-name* 'test-+))
    (check
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -4))))

(defun test-* ()
  (let ((*test-name* 'test-*))
    (check
      (= (* 2 2) 4)
      (= (* 3 5) 15))))

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

;;; full abstraction
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
    (let ((*test-name* ',name))
      ,@body)))

(deftest test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))

(deftest test-math ()
  (combine-results
    (test-arithmetic)))

;;; better deftest
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
    (let ((*test-name* (append *test-name* (list ',name))))
      ,@body)))
