;; Firstly, we have the idea of "streams" or "pipes": functions called
;; repeatedly to produce different values: such a function can easily
;; express eg. any infinite series. Of course, for this to to be of
;; any utility, we need a utility to do this calling in a
;; terse-yet-expressive manner. This is what ---> does:

(defmacro ---> (size composition &rest arguments)
  (let ((express (gensym "EXPRESS"))
        (unexpressed (gensym "UNEXPRESSED"))
        (expressed (gensym "EXPRESSED"))
        (expression-arguments (gensym "EXPRESSION-ARGUMENTS")))
    `(let ((,expression-arguments (list ,@arguments)))
       (labels ((,express (,expressed ,unexpressed)
                  (if (zerop ,unexpressed)
                      ,expressed
                      (,express (append ,expressed (list (apply ,composition ,expression-arguments))) 
                                (- ,unexpressed 1)))))
         (,express nil ,size)))))

;; This macro expands into an elegant recursion that constructs a list
;; by repeatedly calling a function with the results of evaluating
;; arguments. The last bit is the key to its power:

;; no need for this
;; (let ((stream (make-stream))) (---> *n* (composition) stream))

;; just this is enough
;; (---> *n* (composition) (make-stream))

;; Next, we need tools for writing streams. What we would like to be
;; able to do is:

;; (defun natural-number-stream () (<--- 0 1+))

;; Happily, <--- is simple enough:

(defmacro <--- (start step)
  (let ((value (gensym "VALUE")))
    `(let ((,value ,start))
       (lambda () (prog1 ,value (setf ,value (,step ,value)))))))

;; Finally, we want to be able to express our composition nicely. Say,
;; we would like to put (lambda (x) (- (funcall x)) in a better way.

;; This would be nice: no extra parens!
;; (<- -)

;; Again, easy enough. This is a slight extension to my earlier <-,
;; which deals with the _ implicitly, and automatically handles
;; functions as arguments, so that it can be used with streams like
;; above.

(defmacro <- (operation &rest arguments)
  (let ((processed-arguments (if (member '_ arguments) arguments (append arguments '(_)))))
    `(lambda (_) 
       (setf _ (if (functionp _) (funcall _) _))
       (,operation ,@processed-arguments))))

;; That's all there is to it! Now we can express mappings over natural
;; numbers in a clear and concise manner:

;; (---> 3 (<- -) (<--- 0 1+)) ; => (0 -1 -2)

;; Where are <-- and --> you might ask, naturally enough. I'm
;; reserving them for implementing general purpose lazy evaluation...
