;;; stream (or called pipe)
;;; This implementation is very efficient but less elegant and general than that in SICP.
;;; Problem: note that here we use an assumption that
;;; THE ELEMENT IN THE STREAM WILL NEVE BE A FUNCTION. 

(defconstant +empty-stream+ nil)

(defun stream-null? (stream)
  (null stream))

(defmacro make-stream (head tail)
  "Create a stream by evaluating head and delaying tail."
  `(cons ,head #'(lambda () ,tail)))

(alias cons-stream make-stream)

(defun stream-car (stream)
  "Return the first element of a stream."
  (first stream))

(alias stream-first stream-car)
(alias head stream-car)

;;; we assume that the value in the stream will NEVER be a function!!
(defun stream-cdr (stream)
  "Return tail of a stream or a list, and destructively update
the tail if it is a function."
  (if (functionp (rest stream))
      (setf (rest stream) (funcall (rest stream)))
      (rest stream)))

(alias stream-rest stream-cdr)
(alias tail stream-cdr)

(defun stream-elt (stream n)
  "The nth element of a stream, 0-based."
  (if (= n 0)
      (head stream)
      (stream-elt (tail stream) (decf n))))

(defun stream-nth (n stream)
  "The nth element of a stream, 0-based."
  (stream-elt stream n))

(alias stream-ref stream-elt)


;;; instances,
(defun integers (&optional (start 0) end)
  "A stream of integers from START to END.
if ENG is nil, this is an infinite stream."
  (if (or (null end) (<= start end))
      (make-stream start (integers (+ start 1) end))
      nil))

(defun enumerate (stream &key count key (result stream))
  "Go through all (or count) elements of a stream,
possibly applying the KEY function. (Try PRINT.)"
  ;;Returns RESULT, which defaults to the stream itself.
  (if (or (eq stream +empty-stream+) (eql count 0))
      result
      (progn
        (unless (null key) (funcall key (head stream)))
        (enumerate (tail stream) :count (if count (- count 1))
                   :key key :result result))))

(alias walk-stream enumerate)
(alias stream-mapc enumerate)

(defun stream-mapcar (fn stream)
  "Map fn over stream, delaying all but the first fn call."
  (if (eq stream +empty-stream+)
      +empty-stream+
      (make-stream (funcall fn (head stream))
                   (stream-map fn (tail stream)))))

;;; the usage of map is diff to the original map. but consistent with mapcar
(alias stream-map stream-mapcar)

(defun stream-for-each (fn stream)
  "Similar to stream-map, but only return 'done when finished.
 Used in printing or testing."
  (if (stream-null? stream)
      'done
      (progn (funcall fn (head stream))
             (stream-for-each fn (tail stream)))))

