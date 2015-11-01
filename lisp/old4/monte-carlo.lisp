(defparameter *default-element-type* 'single-float)

(defmacro monte-carlo (code-to-run run-times &key (element-type *default-element-type*))
  "A macro for taking code which has a random component and running the
    same piece of code n times and collecting the results."
  (let ((col (gensym)))
    `(let* ((run-times ,run-times)
	    (,col (make-array run-times :element-type ',element-type)))
       (dotimes (i run-times ,col)
	 (setf (aref ,col i) ,code-to-run)))))


#|

Example: Computing the expectation of a random product and sum
The goal is to compute the expectation of E(X) * E(Y), where X and Y are deviates from a uniform distribution on the interval [0,1]. The function (random 1.0) will generate a deviate from a uniform distribution on the interval [0,1].

A simple function for computing the mean of a list of values needs to be defined.

    
(defun mean (nlist) (/ (reduce #'+ nlist) (length nlist)))
    

    CL-USER> (mean (monte-carlo (* (random 1.0) (random 1.0)) 10))
    0.28285187
    CL-USER> (mean (monte-carlo (* (random 1.0) (random 1.0)) 100))
    0.24429739
    CL-USER> (mean (monte-carlo (* (random 1.0) (random 1.0)) 1000))
    0.24217983
    CL-USER> (mean (monte-carlo (* (random 1.0) (random 1.0)) 10000))
    

As easy as it was to compute the expectation of the product of two random variables the sum can be computed.

    CL-USER> (mean (monte-carlo (+ (random 1.0) (random 1.0)) 10))
    1.0999103
    CL-USER> (mean (monte-carlo (+ (random 1.0) (random 1.0)) 100))
    1.043648
    CL-USER> (mean (monte-carlo (+ (random 1.0) (random 1.0)) 1000))
    1.0064476
    CL-USER> (mean (monte-carlo (+ (random 1.0) (random 1.0)) 10000))
    0.9966

#|
