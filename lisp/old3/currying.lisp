(defmacro define-namespace (namespace operator)
  `(defmacro ,namespace (names &body code)
    (flet ((make-macro-def(name)
             `(,name (&rest args) `(,',',operator ,',name ,@args))))
      `(macrolet ,(mapcar #'make-macro-def names) ,@code))))

(define-namespace with-functions funcall)

(defmacro defcurry (name primary-lambda-list
                    secondary-lambda-list
                    &body code)
  `(defun ,name ,primary-lambda-list
    (lambda ,secondary-lambda-list ,@code)))

;;;I continue by writing

(defun show-image (f)
  (format t "~&~{~A~%~}" (render f)))

(defun render (f)
  "Build list of strings to visualise f"
  (loop for y below 40
        collect (coerce (loop for x below 40
                              collect (if (funcall f x y)
                                          #\*
                                          #\Space))
                        'string)))

(defun circle (x y)
  (< (+ (expt x 2)
        (expt y 2))
     1))

(defcurry scale (factor f)(x y)
  (funcall f (/ x factor) (/ y factor)))

(defcurry translate (dx dy f)(x y)
  (funcall f (- x dx) (- y dy)))

(defcurry combine (op f g)(x y)
  (with-functions (op f g)
    (op (f x y)(g x y))))

(defun ring (inner-radius outer-radius)
  (combine (complement #'eql)
           (scale inner-radius #'circle)
           (scale outer-radius #'circle)))

(defun add (f g)
  (combine (lambda(a b)(or a b)) f g))

(defun the-eye ()
  (let ((pupil (translate 10 15 (ring 5 6)))
        (ball (translate 15 15 (ring 10 12))))
    (add pupil ball))) 
