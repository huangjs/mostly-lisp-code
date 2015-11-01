;;; list-based
#|
(declaim (inline make-queue queue-elements empty-queue-p
                 queue-front dequeue enqueue))

(defun make-queue ()
  (let ((q (list nil)))
    (cons q q)))

(defun queue-elements (q)
  (declare (type cons q))
  (cdar q))

(defun empty-queue-p (q)
  (declare (type cons q))
  (null (cdar q)))

(defun queue-front (q)
  (declare (type cons q))
  (cadar q))

(defun dequeue (q)
  (declare (type cons q))
  (car (setf (car q) (cdar q))))

(defun enqueue (item q)
  (declare (type cons q))
  (setf (cdr q) (setf (cddr q) (list item))))
|#


;;; vector based
(declaim (inline empty-queue-p queue-front dequeue enqueue)
         (optimize speed))

(defstruct q 
  (front 0 :type fixnum)
  (end 0 :type fixnum)
  (size 0 :type fixnum)
  (elements #() :type simple-vector))

(defun make-queue (&key (size 20))
  (declare (type index-type size))
  (make-q :front (1- size)
          :end (1- size)
          :size size
          :elements (make-sequence 'simple-vector size)))

(defun empty-queue-p (q)
  (declare (type q q))
  (= (q-front q) (q-end q)))

(defun queue-front (q)
  (declare (type q q))
  (aref (q-elements q) (q-front q)))

(defun dequeue (q)
  (declare (type q q))
  (prog1 (aref (q-elements q) (q-front q))
    (setf (q-front q) (sb-ext:truly-the fixnum (1- (q-front q))))))

(defun enqueue (item q)
  (declare (type q q))
  (setf (svref (q-elements q) (q-end q)) item)
  (when (minusp (the fixnum (setf (q-end q) (sb-ext:truly-the fixnum (1- (q-end q))))))
    (shift-queue q)))

(defun shift-queue (q)
  (declare (type q q))
  (let* ((elements (q-elements q))
         (new elements))
    (when (> (q-front q) (floor (q-size q) 2))
      (setf new (make-sequence 'simple-vector (sb-ext:truly-the fixnum (* 2 (q-size q)))))
      (setf (q-elements q) new)
      (setf (q-size q) (sb-ext:truly-the fixnum (* 2 (q-size q)))))
    (setf (q-end q) (sb-ext:truly-the fixnum (- (q-size q) 2 (q-front q))))
    (replace new elements :start1 (1+ (q-end q)))
    (setf (q-front q) (sb-ext:truly-the fixnum (1- (q-size q))))))

