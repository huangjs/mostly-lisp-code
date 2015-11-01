(defmacro with-processes ((name 
                             (pid num-processes)
                             (work-item work-queue)) &body body)
    (let ((process-fn (gensym))
          (items (gensym))
          (items-lock (gensym)))
      `(let ((,items (copy-list ,work-queue))
             (,items-lock (make-lock)))
         (flet ((,process-fn (,pid)
                  (let ((,work-item nil))
                    (loop
                      (with-lock-grabbed (,items-lock)
                        (setq ,work-item (pop ,items)))
                      (when (null ,work-item)
                        (return))
                      ;;(format t "~&running id ~D~%" ,pid) 
                      ,@body))))
           (dotimes (i ,num-processes)
             ;;(format t "~&creating id ~D~%" ,id) 
             (process-run-function
              (format nil "~A-~D" ,name i)
              #',process-fn
              i))))))

; (with-processes ("Test"
;                  (id 3)
;                  (item '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
;   (format t "~&id ~D item ~A~%" id item)
;   (sleep (random 1.0)))
