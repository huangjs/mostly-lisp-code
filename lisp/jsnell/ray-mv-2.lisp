(declaim (optimize (speed 3) (safety 0) (space 0) (debug 0)
                   #+sbcl (sb-ext:inhibit-warnings 3)))

(declaim (inline vec intersect ray-trace ray-sphere))

(defconstant infinity most-positive-double-float)
(defconstant delta (sqrt double-float-epsilon))

(deftype vec () '(simple-array double-float (3)))

(defun vec (x y z)
  (let ((vec (make-array 3 :element-type 'double-float)))
    (setf (aref vec 0) x
          (aref vec 1) y
          (aref vec 2) z)
    vec))

(defmacro mvec (form)
  `(multiple-value-bind (x y z) ,form
    (vec x y z)))

(defmacro unvec (form)
  (let ((var (gensym)))
    `(let ((,var ,form))
       (values (x ,var) (y ,var) (z ,var)))))

(macrolet ((define-vec-accessor (name index)
               `(progn
                 (declaim (inline ,name (setf ,name)))
                 (defun ,name (vec)
                   (aref vec ,index))
                 (defun (setf ,name) (val vec)
                   (setf (aref vec ,index) val)))))
  (define-vec-accessor x 0)
  (define-vec-accessor y 1)
  (define-vec-accessor z 2))

(defvar *zero-vec* (vec 0d0 0d0 0d0))

(defmacro def ((name params &body body)
               (mname &rest mparams)
               (wname &rest wparams))
  `(progn
    (declaim (inline ,name ,wname))
    (defun ,name ,params ,@body)
    (defmacro ,mname ,(mapcar #'car mparams)
      ,(loop with inner = (list name)
             with body = ``,',inner
             with all-names = nil
             for (form count) in (reverse mparams)
             for names = (loop repeat count collect (gensym))
             do
             (setf all-names (append all-names names))
             (setf body ``(multiple-value-bind ,',(reverse names)
                           ,,form ,,body))
             finally
             (setf (cdr inner) (reverse all-names))
             (return body)))
    (defun ,wname ,(mapcar #'car wparams)
      (,mname ,@(mapcar #'cadr wparams)))))

(def (v*/values (s rx ry rz)
       (values (* s rx)
               (* s ry)
               (* s rz)))
     (mv* (s 1) (form 3))
     (v* (s s) (r (unvec r))))

(def (dot/values (ax ay az bx by bz)
       (+ (* ax bx)
          (* ay by)
          (* az bz)))
     (mdot (a 3) (b 3))
     (dot (a (unvec a)) (b (unvec b))))

(def (unitise/values (rx ry rz)
       (v*/values (/ 1 (sqrt (abs (dot/values rx ry rz rx ry rz))))
                  rx ry rz))
     (munitise (form 3))
     (unitise (r (unvec r))))

(def (len/values (rx ry rz)
       (sqrt (dot/values rx ry rz
                         rx ry rz)))
     (mlen (form 3))
     (len (r (unvec r))))

(defmacro defvfun (name mname wname op)
  `(def (,name (ax ay az bx by bz)
         (values (,op ax bx)
          (,op ay by)
          (,op az bz)))
    (,mname (a 3) (b 3))
    (,wname (a (unvec a)) (b (unvec b)))))

(defvfun v+/values mv+ v+ +)
(defvfun v-/values mv- v- -)

(defstruct ray
  (orig *zero-vec* :type vec)
  (dir *zero-vec* :type vec))

(defstruct sphere
  (center *zero-vec* :type vec)
  (radius 0d0 :type double-float))

(defstruct (group (:include sphere))
  children)

(defun ray-sphere (ray sphere)
  (multiple-value-bind (x y z)
      (v- (sphere-center sphere)
          (ray-orig ray))
    (let* ((dir (ray-dir ray))
           (b (dot/values x y z
                          (x dir) (y dir) (z dir)))
           (disc (+ (- (* b b)
                       (dot/values x y z
                                   x y z))
                    (expt (sphere-radius sphere) 2))))
      (if (< disc 0d0)
          infinity
          (let ((disc (sqrt disc)))
            (let ((t2 (+ b disc))
                  (t1 (- b disc)))
              (cond ((< t2 0d0) infinity)
                    ((> t1 0d0) t1)
                    (t t2))))))))

(defun intersect (ray scene)
  (labels ((aux (lam scene nx ny nz)
             (declare (double-float lam nx ny nz))
             (let ((lamt (ray-sphere ray scene))) 
               (etypecase scene
                 (group 
                  (when (< lamt lam)
                    (dolist (kid (group-children scene))                  
                      (setf (values lam nx ny nz)
                            (aux lam kid nx ny nz))))
                  (values lam nx ny nz))                          
                 (sphere
                  (if (>= lamt lam)
                      (values lam nx ny nz)
                      (multiple-value-bind (x y z)
                          (munitise (mv- (mv+ (unvec (ray-orig ray))
                                              (v* lamt (ray-dir ray)))
                                         (unvec (sphere-center
                                                 scene))))
                        (values lamt x y z))))))))
    (aux infinity scene 0d0 0d0 0d0)))

(defparameter cached-ray (make-ray))

(defun ray-trace (light anti-light ray scene)
  (multiple-value-bind (lam x y z)
      (intersect ray scene)
    (if (= lam infinity)
        0d0
        (let ((g (mdot (values x y z)
                       (unvec light))))
          (if (>= g 0d0)
              0d0
              (let ((p (mvec (mv+ (mv+ (unvec (ray-orig ray))
                                       (v* lam (ray-dir ray)))
                                  (v*/values delta x y z)))))
                (setf (ray-orig cached-ray) p
                      (ray-dir cached-ray) anti-light)
                (if (< (intersect cached-ray scene)
                       infinity)
                    0d0
                    (- g))))))))

(defun create (n c r)
  (declare (double-float r) (fixnum n))
  (let ((obj (make-sphere :center c :radius r)))
    (if (= n 1)
        obj
        (let ((rt (* 3.0d0 (/ r (sqrt 12.0d0)))))
          (labels ((kid (x z)
                     (create (1- n)
                             (mvec (mv+ (unvec c)
                                        (values x rt z)))
                             (/ r 2.0d0)))
                   (bound (obj c r)
                     (typecase obj
                       (group
                        (dolist (kid (group-children obj))
                          (setf (values c r)
                                (bound kid c r)))
                        (values c r))
                       (sphere
                        (values c (max r (+ (mlen (v- (sphere-center obj) c))
                                            (sphere-radius obj))))))))
            (let ((kids (list obj
                              (kid (- rt) (- rt))
                              (kid (- rt) rt)
                              (kid rt (- rt))
                              (kid rt rt))))
              (let ((c (mvec (v+ c (vec 0d0 r 0d0))))
                    (r r))
                (dolist (kid kids)
                  (setf (values c r)
                        (bound kid c r)))
                (make-group :center c
                            :radius r
                            :children kids))))))))

(defun main (level file-name n)
  (declare (fixnum level n))
  (let* ((scene (create level (vec 0d0 -1.0d0 0d0) 1.0d0))
         (ss 4d0)
         (light (mvec (unitise/values -1.0d0 -3.0d0 2.0d0)))
         (anti-light (mvec (v* -1.0d0 light)))
         (ray (make-ray :orig (vec 0d0 0d0 -4.0d0)))
         (-n/2 (/ n -2.0d0))
         (1-n/2 (1- (/ n 2.0d0))))
    (with-open-file (s file-name
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede
                       :if-does-not-exist :create
                       :direction :output)
      (loop for c across (format nil "P5~%~A ~A~%255~%" n n) do
            (write-byte (char-code c) s))
      (loop for y of-type double-float from 1-n/2 downto -n/2 do
            (loop for x of-type double-float from -n/2 to 1-n/2
                  for g of-type double-float = 0d0 do
                  (loop for dx from x below (1+ x) by (/ ss) do
                        (loop for dy from y below (1+ y) by (/ ss)
                              for dir = (ray-dir ray)
                              do
                              (multiple-value-bind (x y z)
                                  (unitise/values dx dy (float n 0d0))
                                (declare (double-float x y z))
                                (setf (x dir) x
                                      (y dir) y
                                      (z dir) z))
                              (incf g (ray-trace light anti-light ray scene))))
                  (write-byte (round (* 255.0d0 (/ g (* ss ss)))) s))))))

