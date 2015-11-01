(declaim (optimize (speed 3) (debug 1) (safety 1)))

(deftype state () '(unsigned-byte 8))
(deftype brain () `(simple-array state (* *)))
(deftype neighbours () `(simple-array state (8)))

(defconstant +off+ 0)
(defconstant +on+ 1)
(defconstant +dying+ 2)

(declaim (inline make-brain)
         (ftype (function (fixnum fixnum) brain) make-brain))
(defun make-brain (w h)
  (make-array (list h w) :element-type 'state :initial-element +off+))

(declaim (ftype (function (fixnum fixnum) brain) make-initialised-brain))
(defun make-initialised-brain (w h)
  (let ((cells (make-brain w h))
        (mid (floor w 2)))
    (setf (aref cells 0 mid) +on+)
    (setf (aref cells 0 (1+ mid)) +on+)
    cells))

(declaim (inline rules)
         (ftype (function (state neighbours) state) rules))
(defun rules (state neighbours) 
  (declare (optimize (safety 0)))
  (case state
    (#.+on+  +dying+)
    (#.+dying+ +off+)
    (t (if (= 2 (loop for i across neighbours counting (eq i +on+))) +on+ +off+))))

(declaim (inline neighbours)
         (ftype (function (brain fixnum fixnum) neighbours) neighbours))
(defun neighbours (cells x y) 
  (let* ((mx (1- (array-dimension cells 1)))
         (my (1- (array-dimension cells 0)))
         (l (if (zerop x) mx (the fixnum (1- x))))
         (r (if (= x mx) 0 (the fixnum (1+ x))))
         (u (if (zerop y) my (the fixnum (1- y))))
         (d (if (= y my) 0 (the fixnum (1+ y))))
         (v (load-time-value (make-array 8 :element-type 'state :initial-element +off+))))
    (declare (optimize (safety 0))
             (type neighbours v))
    (setf (aref v 0) (aref cells u l)
          (aref v 1) (aref cells u x)
          (aref v 2) (aref cells u r)
          (aref v 3) (aref cells y l)
          (aref v 4) (aref cells y r)
          (aref v 5) (aref cells d l)
          (aref v 6) (aref cells d x)
          (aref v 7) (aref cells d r)) 
    v))

(declaim (ftype (function (brain) brain) evolve))
(defun evolve (src)
  (let* ((w (array-dimension src 1))
         (h (array-dimension src 0))
         (dst (make-brain w h)))
    (declare (type brain dst))
    (loop for j below h
          do
       (loop for i below w
             do
          (setf (aref dst j i)
                (rules (aref src j i) (neighbours src i j)))))
    dst))

(defun simulate (steps initial)
  (declare (type brain initial)
           (fixnum steps))
  (loop with brain = initial
        repeat steps
        do (setf brain (evolve brain))
        finally (return brain)))

(defun benchmark ()
  (format *trace-output* "Benchmarking on ~A ~A~%"
          (lisp-implementation-type)
          (lisp-implementation-version))
  ;; Warmup.
  (simulate 10000 (make-initialised-brain 16 16))
  (loop
    for (w h i) in '((32    32  32768)
                     (64    64  8192)
                     (128  128  2048)
                     (256  256  512)
                     (512  512  128)
                     (1024 1024 32)
                     (2048 2048 8)
                     (4096 4096 2))
    do (let ((initial (make-initialised-brain w h)))
         (format *trace-output* "*** ~Dx~D ~D iteration~:P ***~%" w h i)
         (time (simulate i initial))
         (finish-output *trace-output*)))
  (values))

