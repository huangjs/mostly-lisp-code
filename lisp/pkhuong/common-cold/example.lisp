;;; Each form should be C-c C-c ed in Slime one at a time.
;;; The last two forms need to be executed in order, after
;;; the function definition; it would also make sense to
;;; put them in a function, but this is just a teeny example.

(cl:in-package "COMMON-COLD")

(defun test (&optional (val 0))
  (mdlet ((the-count val))
    (send/suspend (k) ; k = a string representing the URL for the continuation
      (cl-who:with-html-output-to-string (*standard-output*)
        (:html (:head (:title "TEST"))
               (:body
                (cl-who:fmt "count ~A~%" the-count)
                (:a :href k "to-cont")))))
    (send/suspend (k)
      (cl-who:with-html-output-to-string (*standard-output*)
        (:html (:head (:title "DONE"))
               (:body (cl-who:fmt "count ~A~%" the-count)
                      (:a :href k "restart")))))
    (test (1+ the-count))))

(ensure-all-builders) ; compile all the program fragments

(push (hunchentoot:create-prefix-dispatcher 
       "/~pkhuong/dyn/cont/"
       (make-continuation-handler 'test ; begin with test if URL
                                  "/~pkhuong/dyn/cont/" ; == this
                                  ))
      hunchentoot:*dispatch-table*)
