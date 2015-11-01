(defun shift* (k entry)
  ;; The default behavior of shift is to throw to
  ;; reset by discarding k; only by calling the second
  ;; argument to entry can we take that continuation.
  (funcall entry
           #'identity
           (lambda (k1 val)
             (funcall k1 (funcall k val)))))

(defun reset* (k thunk)
  ;; This is a barrier;
  ;; no captured continuation can "see" k
  (funcall k (funcall thunk #'identity)))

;; and function to play with

(defun cps-mapcar (k fun list)
  (if (atom list)
      (funcall k nil)
      (funcall fun
               (lambda (item)
                 (cps-mapcar (lambda (rest)
                               (funcall k (cons item rest)))
                             fun
                             (cdr list)))
               (car list))))

;; try
(reset* #'identity
        (lambda (k)
          (cps-mapcar k
                      (lambda (k item)
                        (shift* k (lambda (k f)
                                    (funcall k (cons item f)))))
                      '(1 2 3)))) 
