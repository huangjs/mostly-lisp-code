(defclass foo ()
  ((fred   :initarg :fred   :reader fred)
   (barney :initarg :barney :reader barney)))

#|
CL-USER> (equal (make-instance 'foo :fred 'fred :barney 'barney)
                (make-instance 'foo :fred 'fred :barney 'barney))
NIL
CL-USER> (equiv:object= (make-instance 'foo :fred 'fred :barney 'barney)
                        (make-instance 'foo :fred 'fred :barney 'barney))
NIL
|#

;; Now, I promise that I will not modify instances of FOO. Also, both of its slots are relevant to determine equivalence between two instances.


(defmethod equiv:object-frozenp ((foo foo))
  t)

(defmethod equiv:object-constituents ((type (eql 'foo)))
  (declare (ignore type))
  (load-time-value (list #'fred #'barney)))

#|
However, this might not be enough:

CL-USER> (equiv:object= (make-instance 'foo :fred (list 'fred)
									   :barney (list 'barney))
                        (make-instance 'foo :fred (list 'fred)
									   :barney (list 'barney)))
NIL
|#

#|
Lists are build from cons cells and those are mutable, and so are our instances then, by proxy. I have to promise that at least I will not mutate the conses pointed to in the slots of our FOO instances:


CL-USER> (equiv:object= (make-instance 'foo :fred (list 'fred)
									   :barney (list 'barney))
                        (make-instance 'foo :fred (list 'fred)
									   :barney (list 'barney))
                        t)
T
|#


