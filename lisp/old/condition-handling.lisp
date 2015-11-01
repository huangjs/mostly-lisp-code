(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
      (make-instance 'log-entry ...)
      (error 'malformed-log-entry-error :text text)))

;;; example
(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
          for entry = (handler-case (parse-log-entry text)
                        (malformed-log-entry-error () nil))
          when entry collect it)))

;;; just call restart
(defun parse-log-file (file)
  (with-open-file (in file :direction :input)
    (loop for text = (read-line in nil nil) while text
          for entry = (restart-case (parse-log-entry text)
                                    (skip-log-entry () nil))
          when entry collect it)))


;;; example
(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
                  #'(lambda (c)
                      (invoke-restart 'skip-log-entry))))
    (dolist (log (find-all-logs))
      (analyze-log log))))

;;; a common practice is to define a function with the same name
(defun skip-log-entry (c)
  (invoke-restart 'skip-log-entry))

;;; and now
(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error #'skip-log-entry))
    (dolist (log (find-all-logs))
      (analyze-log log))))

;;; more safely
(defun skip-log-entry (c)
  (let ((restart (find-restart 'skip-log-entry)))
    (when restart (invoke-restart restart))))

;;; more complex recovery protocols
(defun parse-log-entry (text)
  (if (well-formed-log-entry-p text)
      (make-instance 'log-entry ...)
      (restart-case (error 'malformed-log-entry-error :text text)
                    (use-value (value) value)
                    (reparse-entry (fixed-text) (parse-log-entry fixed-text)))))
;; The name USE-VALUE is a standard name for this kind of restart. no need to define a entry function for it.

;;; continue
(defun log-analyzer ()
  (handler-bind ((malformed-log-entry-error
                  #'(lambda (c)
                      (use-value
                       (make-instance 'malformed-log-entry :text (text c))))))
    (dolist (log (find-all-logs))
      (analyze-log log))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Example
;;; search a list for an object, returning true if the object is present and false otherwise.
(defun search-list (goal-item list-to-search)
  (handler-case
      ;; Main body
      (progn (dolist (item list-to-search)
               (when (eq item goal-item)
                 (return-from search-list t)))
             ;; Search has failed, signal an error.
             (error 'search-failure
                    :item goal-item))
    ;; Upon error, just return false.
    (error () nil)))


;;; catch and throw
(defun fn-a ()
  (catch 'fn-a
    (print 'before-fn-b-call)
    (fn-b)
    (print 'after-fn-b-call)))

(defun fn-b ()
  (print 'before-fn-c-call)
  (fn-c)
  (print 'after-fn-c-call))

(defun fn-c ()
  (print 'before-throw)
  (throw 'fn-a 'done)
  (print 'after-throw))

;;; and run (fn-a)
