(in-package #:cl-glut-examples)

(progn (setq glut::*argcp* (cffi:null-pointer) glut::*argv* (cffi:null-pointer)))(glut:init)

(defclass mandelbrot-window (glut:window)
  ((image :initform nil
          :accessor image)
   (width :initarg :width
          :accessor width)
   (height :initarg :height
           :accessor height)
   (need-recalc :initform t
                :accessor need-recalc)
   (real-extents :initarg :real-extents
                 :accessor real-extents)
   (imag-extents :initarg :imag-extents
                 :accessor imag-extents))
  (:default-initargs :pos-x 100 :pos-y 100
                     :mode '(:single :rgb) :title "mandelbrot.lisp"))

(defmethod glut:display-window :before ((w mandelbrot-window))
  ;; Select clearing color.
  (gl:clear-color 0 0 0 0)
  ;; Initialize viewing values.
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))

(defmethod glut:display ((w mandelbrot-window))
  (gl:clear :color-buffer)
  (gl:raster-pos 0 0)
  (when (need-recalc w)
    (format t "Need to recalc...~%")
    (format t "Real: ~A Imag: ~A~%" (real-extents w) (imag-extents w))
    (setf (image w) (calculate-mandelbrot-image (width w) (height w) :real (real-extents w) :imag (imag-extents w) :max-iterations 256))
    (setf (need-recalc w) nil)
    (format t "Done...~%"))
  (gl:draw-pixels (image-width (image w)) (image-height (image w)) :rgb :unsigned-byte (image-1darray (image w)))
  (gl:flush))

(defun rb-mandelbrot (width height &key (max-iterations 1000) (real '(-1.5 1.5)) (imag '(-1.5 1.5)))
  (glut:display-window 
   (make-instance 'mandelbrot-window
                  :width width :height height
                  :real-extents real :imag-extents imag)))
					;                  :image (calculate-mandelbrot-image width height :real real :imag imag :max-iterations max-iterations))))

(defun midpoint (lst)
  (/ (+ (first lst) (second lst))
     2))

(defmethod glut:keyboard ((w mandelbrot-window) key x y)
  (with-slots (real-extents imag-extents) w
    (let ((imag-delta (* 4 (/ (- (second imag-extents) (first imag-extents)) (height w))))
          (real-delta (* 4 (/ (- (second real-extents) (first real-extents)) (width w))))
          (imag-midpoint (midpoint imag-extents))
          (real-midpoint (midpoint real-extents)))
      (case key
        (#\a 
         (incf (first imag-extents) imag-delta)
         (incf (second imag-extents) imag-delta))
        (#\A
         (incf (first imag-extents) (* 10 imag-delta))
         (incf (second imag-extents) (* 10 imag-delta)))
        (#\d 
         (incf (first imag-extents) (- imag-delta))
         (incf (second imag-extents) (- imag-delta)))
        (#\D
         (incf (first imag-extents) (* 10 (- imag-delta)))
         (incf (second imag-extents) (* 10 (- imag-delta))))
        (#\w
         (incf (first real-extents) (- real-delta))
         (incf (second real-extents) (- real-delta)))
        (#\s 
         (incf (first real-extents) real-delta)
         (incf (second real-extents) real-delta))
        (#\S
         (let ((imag-midpoint-distance (- imag-midpoint (first imag-extents)))
               (real-midpoint-distance (- real-midpoint (first real-extents))))
           (setf (first imag-extents) (- (first imag-extents) imag-midpoint-distance))
           (setf (second imag-extents) (+ (second imag-extents) imag-midpoint-distance))
           (setf (first real-extents) (- (first real-extents) real-midpoint-distance))
           (setf (second real-extents) (+ (second real-extents) real-midpoint-distance))))
        (#\W
         (setf (first imag-extents) (midpoint (list (first imag-extents)
                                                    imag-midpoint)))
         (setf (second imag-extents) (midpoint (list imag-midpoint
                                                     (second imag-extents))))
         (setf (first real-extents) (midpoint (list (first real-extents)
                                                    real-midpoint)))
         (setf (second real-extents) (midpoint (list real-midpoint
                                                     (second real-extents))))))
      (case key
        ((#\w #\s #\a #\d #\W #\D #\A #\S)
         (setf (need-recalc w) t)
         (setf (real-extents w) real-extents)
         (setf (imag-extents w) imag-extents)))))
  (glut:post-redisplay))

(defun make-image (w h)
  (let* ((underlying-array (make-array (* w h 3) :element-type '(unsigned-byte 8)
                                       :initial-element 0))
         (displaced-array (make-array (list w h 3) :element-type '(unsigned-byte 8)
                                      :displaced-to underlying-array)))
        
    (list w h underlying-array displaced-array)))

(defun image-width (image)
  (first image))

(defun image-height (image)
  (second image))

(defun image-1darray (image)
  (third image))

(defun image-3darray (image)
  (fourth image))

(defun set-colour (image x y iterations-taken max-iterations)
  (let (depth)
    (if (= iterations-taken max-iterations)
        (setf depth 0)
        (progn
          (setf depth (truncate (* 255 (/ iterations-taken 1000))))))
    (let ((r (min 180 (* 20 depth)))
          (g (min 180 (* 5 depth)))
          (b (min 180 (* 1 depth)))
          (data (image-3darray image)))
      (setf (aref data x y 0) r)
      (setf (aref data x y 1) g)
      (setf (aref data x y 2) b))))

(defun within-radius (z)
  (let ((r (realpart z))
        (i (imagpart z)))
    (<= (+ (* r r) (* i i))
        4)))

(defun calculate-mandelbrot-image (width height &key (max-iterations 1000)
                                   (real '(-1.5 1.5))
                                   (imag '(-1.5 1.5)))
  (let* ((image (make-image width height))
         (low-real (first real))
         (high-real (second real))
         (low-imag (first imag))
         (high-imag (second imag))
         (real-length (- high-real low-real))
         (imag-length (- high-imag low-imag))
         (real-by (/ real-length width))
         (imag-by (/ imag-length height)))
    (flet  ((do-it (start-x end-x start-cr)
              (loop
                 for x from start-x to end-x
                 for cr from start-cr by real-by
                 do
		 (loop 
		    for y below height
		    for ci from low-imag by imag-by
		    do
		    (let* ((c (complex cr ci))
			   (iterations-taken
			    (loop
			       for z = c then (+ (* z z) c)
			       for iteration from 0 below max-iterations
			       while (within-radius z)
			       count iteration)))
		      (set-colour image (truncate x) (truncate y) iterations-taken max-iterations))))))
      (let* ((end-x (truncate (/ width 2)))
             (threads (list (sb-thread:make-thread (lambda ()
                                                     (do-it 0 end-x low-real)))
                            (sb-thread:make-thread (lambda ()
                                                     (do-it (1+ end-x) (1- width) (+ (* end-x real-by) low-real)))))))
        (loop for thread in threads
           do (sb-thread:join-thread thread))))
    image))
