(defun distance (x y)
  (let ((xx (first x))
	(xy (second x))
	(yx (first y))
	(yy (second y)))
    (declare (type (integer 0 4000) xx xy yx yy))
    #I"sqrt((yx - xx)^^2 + (yy - xy)^^2)"))

(defun make-reverse-lookup-tabke (points)
  (iter (with hashtable = (make-hash-table :size (length points)))
	(for p in-vector points)
	(for i from 0)
	(setf (gethash p hashtable) i)
	(finally (return hashtable))))

(defun make-distance-lookup-table (points)
  (map 'vector
       (lambda (p)
	 (map 'vector
	      (lambda (q) (distance p q))
	      points))
       points))

(defun make-sorted-lookup-table (points)
  (map 'vector
       (lambda (row)
	 (subseq (sort (copy-seq row) #'< :key #'second) 1))
       (map 'vector
	    (lambda (row)
	      (map 'vector
		   #'list
		   points
		   row))
	    (make-distance-lookup-table points))))

(defparameter *number-of-points* (length *checkpoints*))
(defparameter *unvisited* (loop for i across *checkpoints* collect i))
(defparameter *visited* '())		; probably in reverse order
(defparameter *reverse-lookup-table* (make-reverse-lookup-tabke *checkpoints*))
(defparameter *distance-lookup-table* (make-distance-lookup-table *checkpoints*))
(defparameter *sorted-lookup-table* (make-sorted-lookup-table *checkpoints*))

(defun initialize ()
  (setf *unvisited* (loop for i across *checkpoints* collect i))
  (setf *visited* '()))

;;; interface

(defun get-index (point)
  (multiple-value-bind (i p)
      (gethash point *reverse-lookup-table*)
    (if p i (error "pointer ~a not found in lookup-table" point))))

(defun get-point (p)
  (aref *checkpoints* p))

(defgeneric lookup-distance (p q)
  (:documentation "lookup distance between p and q, p and q can either be index or point coordinate.")
  (:method ((p integer) (q integer))
    (aref (aref *distance-lookup-table* p) q))
  (:method ((p integer) (q list))
    (aref (aref *distance-lookup-table* p) (get-index q)))
  (:method ((p list) (q integer))
    (aref (aref *distance-lookup-table* (get-index p)) q))
  (:method ((p list) (q list))
    (aref (aref *distance-lookup-table* (get-index p)) (get-index q))))

(defgeneric visited-p (point)
  (:method ((point integer))
    (not (find (get-point point) *unvisited*)))
  (:method ((point list))
    (not (find point *unvisited*))))

(defgeneric visit-point (point)
  (:method ((point list))
    (push point *visited*)
    (setf *unvisited* (remove point *unvisited*))
    point)
  (:method ((point integer))
    (visit-point (get-point point))))

(defun total-length (path)
  (iter (for i in path)
	(for j in (rest path))
	(sum (lookup-distance i j))))

(defgeneric direct-nearest (p m &optional visited)
  (:method ((p integer) m &optional (visited *visited*))
    (iter (for i in-vector (aref *sorted-lookup-table* p))
	  (with counter = 0)
	  (while (< counter m))
	  (when (and (not (find (first i) visited)))
	    (progn
	      (incf counter) 
	      (collect (first i))))))
  (:method ((p list) m &optional (visited *visited*))
    (direct-nearest (get-index p) m visited))
  (:documentation   "return the direct nearest points to the point of p."))

;;; search function
(defun next-searches (p n m
		      &optional (visited (union (list (get-point p)) *visited*)))
  "Search the best path start from the pth point, through points which are not visited. Generally, use beam-search algorithm, search n-steps, with every step m pointers. Then choose the minimal cost path and move to the next point. So every step it will compare m^n times." 
  (if (or (<= n 0) (>= (length visited) *number-of-points*))
      '(())
      (mapcan
       (lambda (next-point)
	 (mapcar (lambda (q)
		   (cons next-point q))
		 (next-searches (get-index next-point) (1- n) m (cons next-point visited))))
       (direct-nearest p m visited))))

(defun solve (&key (iteration 10))
  (labels ((random-choose ()
	     (nth (random (length *unvisited*)) *unvisited*))
	   (doit ()
	     (iter (for p initially (get-index (visit-point (random-choose)))
			then (get-index next-point))
		   (until (null *unvisited*))
		   (for next-point =
			(first (first (sort (next-searches p 5 5) #'<
					    :key #'total-length))))
		   (visit-point next-point)))
	   (step-distance (path)
	     (mapcar #'distance
		     (cons (first path) path)
		     path)))
    ;; get 10 solution start randomly
    (iter (for i below iteration)
	  (progn
	    (initialize)
	    (doit))
	  (collect (list #1=(copy-seq *visited*) 
			 (step-distance #1#)
			 (total-length #1#))))))
