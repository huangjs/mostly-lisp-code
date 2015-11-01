;; Decode movie to frames:
;;     mplayer -ao null -vo jpeg <original>
;; Encode frames to movie:
;;     mencoder "mf://*.jpg" -mf fps=25 -o <output> -ovc lavc -lavcopts vcodec=mpeg4

(load "jpeg")

(defun decode-rgb (rgb)
  (values (logand rgb #x0000ff)
          (ash (logand rgb #x00ff00) -8)
          (ash (logand rgb #xff0000) -16)))

(defun encode-rgb (r g b)
  (+ (floor r) (ash (floor g) 8) (ash (floor b) 16)))

(defun average-picture ()
  (multiple-value-bind (width height)
      (jpeg:get-image-size "00000001.jpg")
    (let ((red-sums (make-array (list width height)))
          (green-sums (make-array (list width height)))
          (blue-sums (make-array (list width height)))
          (result (make-array (* width height)))
          (i 0))
      (loop
         (incf i)
         (format t "Processing frame ~A~%" i)
         (force-output)
         (let ((image (jpeg:read-file (format nil "~8,'0d.jpg" i))))
           (dotimes (x width)
             (dotimes (y height)
               (multiple-value-bind (r g b)
                   (decode-rgb (aref image (+ x (* y width))))
                 (incf (aref red-sums x y) r)
                 (incf (aref green-sums x y) g)
                 (incf (aref blue-sums x y) b))
               (setf (aref result (+ x (* y width)))
                     (encode-rgb (/ (aref red-sums x y) i)
                                 (/ (aref green-sums x y) i)
                                 (/ (aref blue-sums x y) i))))))
         (jpeg:write-file (format nil "out~8,'0d.jpg" i) result width height)))))
