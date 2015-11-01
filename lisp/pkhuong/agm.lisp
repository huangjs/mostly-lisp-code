;;;; An implementation of Nesterov's Accelerated gradient method,
;;;; as described in "Gradient methods for minimizing composite
;;;; objective function", Yu. Nesterov, 2007.
(defpackage "AGM"
    (:use "CL" "MATLISP" #+sbcl "SB-EXT")
  (:shadowing-import-from "MATLISP" "REAL")
  (:export "MAKE-AGM-INSTANCE" "SOLVE"

           "VALUE" "GRAD"
           "PROJECT" "MAP-GRADIENT"

           "DISTANCE" "DIST-N" "DIST-B" "DIST-S"
           "DISTANCE-MIN"

           "ADD-FUNCTION-TO-HOOK" "REMOVE-FUNCTION-FROM-HOOK"
           "REGISTER-HOOK" "WITH-HOOKS"))
(in-package "AGM")


;;;; Random utility noise.
(deftype index ()
  `(mod ,array-total-size-limit))

(defmacro missing (name)
  `(error "Missing initial value for slot ~S" ',name))

(declaim (inline ddot))
(defun ddot (x y)
  (the (values double-float &optional) (dot x y)))

;;;; Logging hooks
(defvar *hooks* (make-hash-table #+sbcl :weakness #+sbcl :key)
  "Hash table of hook name (symbol) -> mutable cell (cons)")
(defun %ensure-hook (name)
  (or (gethash name *hooks*)
      (setf (gethash name *hooks*) (list nil))))

(defun call-hooks (hooks &rest arguments)
  (dolist (hook hooks)
    (apply hook arguments)))

(defmacro defhook (name (&rest arguments))
  (declare (ignore arguments))
  (let ((note-symbol (intern (format nil "~A-~A" 'note name)))
        (hooks-form  `(car (load-time-value (%ensure-hook ',name)))))
    `(progn
       (defun ,note-symbol (&rest args)
         (let ((hooks ,hooks-form))
           (when hooks
             (apply 'call-hooks hooks args))))
       (define-compiler-macro ,note-symbol (&rest args)
         (let ((hooks (gensym "HOOKS")))
           `(let ((,hooks ,',hooks-form))
              (when ,hooks
                (funcall 'call-hooks ,hooks ,@args))))))))

(defun add-function-to-hook (hook function)
  (let ((place (%ensure-hook hook)))
    (push function (car place))
    hook))

(defun remove-function-from-hook (hook function)
  (let ((place (%ensure-hook hook)))
    (setf (car place) (delete function (car place) :count 1))
    hook))

(defmacro register-hook (name (&rest arguments) &body body)
  `(add-function-to-hook ',name (lambda (,@arguments)
                                  ,@body)))

(defmacro with-hooks ((&rest hooks)
                      &body body)
  (let ((functions (mapcar (lambda (hook)
                             (gensym (format nil "FUNCTION-FOR-~A" (car hook))))
                           hooks)))
    `(let ,(mapcar (lambda (function hook)
                     (destructuring-bind (hook arguments &rest body)
                         hook
                       (declare (ignore hook))
                       `(,function (lambda ,arguments ,@body))))
            functions hooks)
       (unwind-protect
            (progn
              ,@(mapcar (lambda (function hook)
                          `(add-function-to-hook ',(car hook) ,function))
                      functions hooks)
              ,@body)
         ,@(mapcar (lambda (function hook)
                          `(remove-function-from-hook ',(car hook) ,function))
                      functions hooks)))))

;;;; Nesterov's accelerated gradient method (AGM) efficiently
;;;; minimises a convex function phi = f + psi.
;;;;
;;;; ``efficiently'': the precision grows quadratically with
;;;; the number of iteration and function/gradient evaluations.

;;; f is assumed to be ``nice'' (convex differentiable, with
;;; a Lipschitz continuous gradient), but only described by a
;;; black box that provides values and gradients at points.


;;;; Minimal black box interface
(defgeneric value (fun x)
  (:documentation "Evaluate FUN at X"))
(defgeneric grad  (fun x)
  (:documentation "Return the gradient or a subgradient of FUN at X."))

(defhook fun-eval  (fun x))
(defhook grad-eval (fun x))

;;;; Interface for psi

;;; Psi is the ``nasty'' but well-known function.  It is
;;; typically used to represent penalties, or constraints,
;;; and can be almost arbitrarily nasty, as long as it is
;;; convex.  However, in addition to the usual value
;;; and gradient, we must also be able to minimize the sum of
;;; phi and a distance function of the form [s/2 x'x + b'x].

(defgeneric project (psi distance Ak)
  (:documentation "Compute argmin (distance + Ak psi), where 
distance is a distance function (c.f. below), and Ak a double."))
(defgeneric map-gradient (psi grad y L)
  (:documentation "Compute argmin (g'(x-y) + L/2 |x-y|^2 + psi).
A default implementation in terms of PROJECT is provided."))

(defhook project (fun distance Ak))
(defhook map-gradient (psi grad y L))

;;;; Distance function

;;; The AGM builds a penalized local approximation of the function
;;; by averaging linear approximations, and an initial 2-norm
;;; penalty.
;;;
;;; The function is of the form:
;;; f~(x) = 1/2|x-x0|^2 + \sum [affine functions]
;;;       = 1/2 x'x + b'x + k (for some b and k)
;;;
;;; More generally, we have to minimise a few of these distance
;;; functions. So, add a scale factor to get:
;;; distance(x) = s/2 x'x + b'x + k
;;;
;;; Finally, we only ever need the minimiser, not its value, so the
;;; constant offset k can be ignored.

#+sbcl
(declaim (maybe-inline %make-distance))
(defstruct (distance
             (:constructor %make-distance (n b &optional (s 1d0)))
             (:conc-name #:dist-))
  (n (missing n) :type index :read-only t)
  ;; This column vector can be mutated during updates.
  (s 1d0         :type (double-float (0d0)))
  (b (missing b) :type real-matrix))

;;;   g'(x-y) + L/2 |x-y|^2
;;; = L/2x'x - Ly'x + g'x + L/2y'y - g'y
;;; = L/2 x'x + (g - Ly)'x + k
(defmethod map-gradient (psi grad y L)
  (declare (real-matrix grad y)
           (type double-float L)
           (inline %make-distance))
  (assert (col-vector-p grad))
  (assert (col-vector-p y))
  (let* ((n (nrows y))
         (d (%make-distance n
                            (axpy (- L) y grad)
                            L)))
    (declare (dynamic-extent d))
    (project psi d 1d0)))

;;; Initial approximation
;;;
;;;   1/2 |x - x0|^2
;;; = 1/2 x'x - x0'x + 1/2 x0'x0
(defun make-distance (x0)
  (declare (type real-matrix x0))
  (assert (col-vector-p x0))
  (let ((n (nrows x0))
        (b (scal -1 x0)))
    (%make-distance n b)))

;;; solve: scale/2 x'x + b'x + scale k
;;;  -> scale x + b = 0
;;;           x = -b/scale = -1/scale b
(defun distance-min (fun)
  (declare (type distance fun))
  (let ((s (dist-s fun))
        (b (dist-b fun)))
    (scal (/ -1d0 s) b)))

;;; Update the approximation with a new linear
;;; function:  a (<g, x-x0> + f_x0)
;;;          = a (g'x + (f_x0 - g'x0))
#+sbcl
(declaim (maybe-inline update-distance))
(defun update-distance (distance a x0 f_x0 grad_x0)
  (declare (type distance distance)
           (type double-float a)
           (type real-matrix x0)
           (type double-float f_x0)
           (type real-matrix grad_x0)
           (ignore f_x0 x0)) ; constant offset isn't used
  (let ((a (* a (dist-s distance))))
    (setf (dist-b distance) (axpy! a grad_x0
                                   (dist-b distance))))
  distance)


;;;; The heart of the method

;;; An AGM instance solves arg min_x f + psi
(defstruct (agm-instance
             (:conc-name #:agm-)
             (:constructor %make-agm-instance (n mu gamma_u gamma_d
                                              x L approx psi fun)))
  ;; #var
  (n  (missing n) :type index :read-only t)
  ;; convexity parameter (lower estimate, >= 0)
  (mu (missing mu) :type (double-float 0d0)
   :read-only t)
  ;; update parameters for the Lipschitz constant estimate
  (gamma_u (missing gamma_u) :type (double-float (1d0))     :read-only t)
  (gamma_d (missing gamma_d) :type (double-float (0d0) 1d0) :read-only t)
  ;; Current point
  (x       (missing x) :type real-matrix)
  ;; Lischitz constant estimate
  (L       (missing L) :type (double-float (0d0)))
  ;; \sum a
  (A       0d0          :type (double-float 0d0))
  ;; penalized approximation
  (approx   (missing approx) :type distance :read-only t)
  (psi      (missing psi)    :read-only t)
  (fun      (missing fun)    :read-only t))

(defmethod value ((agm agm-instance) x)
  (+ (value (agm-fun agm) x)
     (value (agm-psi agm) x)))

(defmethod grad ((agm agm-instance) x)
  (m+ (grad (agm-fun agm) x)
      (grad (agm-psi agm) x)))

;;; Solve a quadratic equation to determine the current step
#+sbcl
(declaim (maybe-inline find-a))
(defun find-a (Ak mu L)
  "Solve a^2/(Ak+a) = 2 (1+mu Ak)/L

  La^2 = 2(1 + mu Ak)(a + Ak)
  La^2 = 2(a + Ak + mu Ak a + mu Ak^2)
  La^2 - 2((mu Ak + 1)a + Ak + mu Ak^2) = 0
  La^2 - 2(mu Ak + 1)a - 2(Ak + mu Ak^2)   = 0
"
  (declare (type double-float Ak mu L))
  (let* ((a L)
         (b (* -2 (1+ (* mu Ak))))
         (c (* -2 (+ Ak (* mu Ak Ak))))
         (x (if (< (abs b) 1d-8) ;; shouldn't happen, ever.
                ;; a x^2 + c = 0
                ;; x = sqrt -c/a
                (sqrt (the (double-float 0d0) (/ (- c) a)))
                (let* ((d (sqrt (the (double-float 0d0)
                                  (- (* b b) (* 4 a c)))))
                       (sgn (if (< b 0d0) -1d0 1d0))
                       (q (* -.5d0 (+ b (* sgn d))))
                       (x1 (/ q a))
                       (x2 (/ c q)))
                  (max x1 x2)))))
    (let ((value (abs (+ (* a x x) (* b x) c))))
      (unless (< value 1d-5)
        (format *error-output* "WARNING: (~S ~A ~A ~A) return value residual: ~A > ~A~%"
                'find-a Ak mu L value 1d-5)))
    x))

;;; One iteration of the AGM method
;;; Read the paper, there's no point repeating half of
;;; its contents here.
(defun solve-1 (agm epsilon)
  (declare (type agm-instance agm)
           (type double-float epsilon))
  (let* ((L   (agm-L agm))
         (Ak  (agm-A agm))
         (mu  (agm-mu agm))
         (fun (agm-fun agm))
         (psi (agm-psi agm))
         (approx (agm-approx agm))
         (x   (agm-x agm))
         (gamma_u (agm-gamma_u agm))
         (upsilon (progn
                    (note-project psi approx Ak)
                    (project psi approx Ak))))
    (declare (type double-float L))
    (flet ((value (fun x)
             (note-fun-eval fun x)
             (value fun x))
           (grad (fun x)
             (note-grad-eval fun x)
             (grad fun x))
           (map-gradient (psi grad_y y L)
             (note-map-gradient psi grad_y y L)
             (map-gradient psi grad_y y L)))
      (multiple-value-bind (a y grad_y)
          ;; Loop until L is large enough
          (loop
            (let* ((a       (find-a Ak mu L))
                   (y       (scal! (/ (+ Ak a))
                                   (m+ (scal Ak x)
                                       (scal a  upsilon))))
                   (grad_y  (grad fun y))
                   (TLy     (map-gradient psi grad_y y L))
                   (grad-estimate (grad agm TLy))
                   (delta-y (m- y TLy)))
              (when (< (norm delta-y) epsilon)
                (return-from solve-1 (values x (grad agm x) (value agm x))))
              (if (< (ddot grad-estimate delta-y)
                     (/ (ddot grad-estimate grad-estimate)
                        L))
                  (setf L (* L gamma_u))
                  (return (values a y grad_y)))))
        (let* ((M      L)
               (x      (map-gradient psi grad_y y M))
               (f_x    (value agm x))
               (grad_x (grad agm x)))
          (update-distance approx a x f_x grad_x)
          (setf (agm-L agm) (* M (agm-gamma_d agm))
                (agm-x agm) x
                (agm-A agm) (+ Ak a))
          (values x grad_x f_x))))))


;;;; Trivial psi: psi(x) = 0.
;;; Equivalent to an unrestricted accelerated gradient method.
(defmethod project ((psi (eql :unbounded)) distance Ak)
  (declare (type distance distance)
           (ignore Ak))
  (distance-min distance))

#+nil
(defmethod map-gradient ((psi (eql :unbounded)) grad y L)
  ;; solve arg min g'(x-y) + L/2 |x-y|^2
  ;;  = arg min g'x + L/2(x'x - 2y'x + y'y)
  ;;  = arg min L/2 x'x + (g - Ly)'x
  ;; -> L x + g - Ly = 0
  ;;      x = (Ly - g)/L
  ;;        = y - g/L
  ;;        = y + -1/L g
  (axpy (/ -1d0 L) grad y))

(defmethod value ((fun (eql :unbounded)) x)
  0d0)

(defmethod grad ((fun (eql :unbounded)) x)
  0d0)


;;;; Simple psi: psi(x) = 0     if x >= 0
;;;;                      infty if x <  0
(defun clamp (matrix)
  (map-matrix! (lambda (x)
                 (declare (type double-float x))
                 (max x 0d0))
               matrix))

(defmethod project ((psi (eql :non-negative)) distance Ak)
  (declare (type distance distance)
           (ignore Ak))
  (clamp (distance-min distance)))

(defmethod map-gradient ((psi (eql :non-negative)) grad y L)
  (clamp (axpy (/ -1d0 L) grad y)))

(defmethod value ((fun (eql :non-negative)) x)
  0d0)

(defmethod grad ((fun (eql :non-negative)) x)
  0d0)

;;;; Now with defaults
(defun make-agm-instance (n fun
                          &key (psi :unbounded)
                               (x0  (make-real-matrix n 1)))
  (%make-agm-instance n 0d0
                      1.5d0 (/ 3d0 4)
                      x0 .1d0
                      (make-distance x0)
                      psi fun))

(defun solve (agm
              &key
              (min-gradient-norm 1d-8)
              (min-delta-x 1d-8))
  (declare (type agm-instance agm)
           (type double-float min-gradient-norm min-delta-x))
  (loop
    (let ((old-x (agm-x agm)))
      (multiple-value-bind (x grad_x f_x)
          (solve-1 agm min-delta-x)
        (when (or (<= (norm grad_x) min-gradient-norm)
                  (<= (norm (m- x old-x)) min-delta-x))
          (return (values f_x x grad_x agm)))))))

;;;; Example ``nice'' function: linear least squares
(defstruct (linear-least-square
             (:constructor %make-lls (A b AtA Atb))
             (:conc-name #:lls-))
  A b
  AtA
  Atb)

(defun make-lls (A b)
  (let* ((At  (transpose A))
         (AtA (m* At A)))
    (%make-lls A b AtA (m* At b))))

(defun make-random-lls (rows variables)
  (let ((A (rand rows variables))
        (b (rand rows 1)))
    (time (make-lls A b))))

(defmethod value ((lls linear-least-square) x)
  (let ((diff (m- (m* (lls-A lls) x)
                  (lls-b lls))))
    (* .5d0 (ddot diff diff))))

(defmethod grad ((lls linear-least-square) x)
  ;; d/dx 1/2 (Ax - b)'(Ax - b)
  ;; = (Ax - b)'A
  ;; = (x'A'A - b'A)
  ;; ~= (A'A x - A'b)
  (axpy! -1 (lls-Atb lls)
         (m* (lls-AtA lls) x)))
