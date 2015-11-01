(defgeneric vertex.arcs (graph vertex)
  (:documentation "Return a list of arc descriptors"))
(defgeneric arc.data (graph arc)
  (:documentation "Given an arc descriptor, return the successor arc and the arc's weight"))

(defgeneric make-vertex-table (graph)
  (:documentation "Return a vertex->value table; consequences are undefined if
                   that table is used with another graph.")
  (:method (graph) (make-hash-table :test #'equalp)))
(defgeneric vertex.val (graph table vertex &optional default)
  (:documentation "Look up the value associated with the vertex in the table. If 
                   none, return default and NIL, the value and T otherwise.")
  (:method (graph table vertex &optional default)
    (gethash vertex table default)))
(defgeneric (setf vertex.val) (value graph table vertex &optional default)
  (:method (value graph table vertex &optional default)
    (declare (ignore default))
    (setf (gethash vertex table) value)))

(defgeneric graph.vertices (graph)
  (:documentation "Return a list of all the vertices in the graph"))

(defgeneric transitive-closure (graph start)
  (:documentation  "Compute the transitive closure of vertex start in graph.")
  (:method (graph start)
    (let ((vertex-set (make-vertex-table graph))
          (vertices   '())
          (todo-set   (make-vertex-table graph))
          (todo       (make-array 16 :adjustable t :fill-pointer 0)))
      (flet ((process-vertex (vertex)
               (unless (vertex.val graph vertex-set vertex)
                 (setf (vertex.val graph vertex-set vertex) t)
                 (push vertex vertices)
                 (dolist (arc (vertex.arcs graph vertex))
                   (let ((succ (arc.data graph arc)))
                     (unless (vertex.val graph todo-set succ)
                       (setf (vertex.val graph todo-set succ) t)
                       (vector-push-extend succ todo)))))))
        (vector-push-extend start todo)
        (setf (vertex.val graph todo-set start) t)
        (loop while (> (length todo) 0)
              do (process-vertex (vector-pop todo))
              finally (return (nreverse vertices)))))))

(defgeneric graph.graphviz (graph
                            &key to-highlight label-fun stream
                            &allow-other-keys)
  (:documentation
   "Output the graph in dotty format to :stream (default *standard-output),
    highlighting vertices in to-highlight (a list), and labelling them with 
    the name computed with label-fun.

    label-fun is either NIL (in which case no label is generated)
    or a function that takes the graph and the vertex to label as an
    argument, and returns either a label (a string), or NIL (no label).")
  (:method (graph
            &key to-highlight
                 label-fun
                 ((:stream *standard-output*) *standard-output*))
    (let ((id-table (make-vertex-table graph))
          (counter  0)
          (vertices (graph.vertices graph)))
      (flet ((vertex-label (vertex)
               (and label-fun
                    (funcall label-fun graph vertex)))
             (vertex-id (vertex)
               (multiple-value-bind (id id-p)
                   (vertex.val graph id-table vertex)
                 (if id-p
                     id
                     (setf (vertex.val graph id-table vertex)
                           (format nil "Vertex~A" (incf counter)))))))
        (format t "strict digraph {~%")
        (dolist (vertex vertices)
          (format t "    ~S [" (vertex-id vertex))
          (let ((label (vertex-label vertex)))
            (when label
              (format t "label = ~S, " label)))
          (when (member vertex to-highlight :test #'equalp)
            (format t "style = bold, shape = hexagon"))
          (format t "];~%"))
        (format t "~%")
        (dolist (vertex vertices)
          (dolist (succ (vertex.arcs graph vertex))
            (let ((succ (arc.data graph succ)))
              (format t "    ~S -> ~S;~%"
                      (vertex-id vertex)
                      (vertex-id succ)))))
        (format t "}~%")))))

(defgeneric shortest-path (graph start end)
  (:documentation
   "Compute a shortest path in graph from start to end, returning
    the path's length, the path, as a list of vertices, and the
    vertex table of vertex to distance from start.")
  (:method (graph start end)
    "Generic method: a label-correcting algorithm with a FIFO for update
     candidates is used to handle arbitrary arc costs (but not negative 
     cycles)."
    (let* ((dist  (make-vertex-table graph))
           (prev  (make-vertex-table graph))
           (queue-set (make-vertex-table graph))
           (queue (list nil)) ; cdr points to the queue data so the cons
           (tail  queue))     ; can be used as a first-class reference
      (labels ((enqueue (x)
                 (when (vertex.val graph queue-set x nil)
                   (return-from enqueue nil))
                 (let ((new-cons (list x)))
                   (setf (cdr tail) new-cons
                         tail       new-cons))
                 nil)
               (dequeue (&optional default)
                 (if (eq tail queue)
                     (values default nil)
                     (let ((vertex (pop (cdr queue))))
                       (when (null (cdr queue))
                         (setf tail queue))
                       (setf (vertex.val graph queue-set vertex) nil)
                       (values vertex t))))
               (distance (vertex)
                 (vertex.val graph dist vertex double-float-positive-infinity))
               ((setf distance) (distance vertex)
                 (setf (vertex.val graph dist vertex) distance))
               (register-path (vertex predecessor new-distance)
                 (let ((old-distance (distance vertex)))
                   (when (< new-distance old-distance)
                     (setf (vertex.val graph prev vertex) predecessor
                           (distance vertex) new-distance)
                     (enqueue vertex))))
               (get-path ()
                 (let ((path (list end)))
                   (loop
                     (multiple-value-bind (pred pred-p)
                         (vertex.val graph prev (first path))
                       (if pred-p
                           (push pred path)
                           (return (values (distance end) path dist))))))))
        (setf (vertex.val graph dist start) 0)
        (setf (vertex.val graph queue-set end) t)
        (enqueue start)
        (loop (multiple-value-bind (head full-p) (dequeue)
                (unless full-p
                  (return (get-path)))
                (let ((head-distance (distance head)))
                  (dolist (arc (vertex.arcs graph head))
                    (multiple-value-bind (succ distance) (arc.data graph arc)
                      (register-path succ head (+ head-distance distance)))))))))))

;; A state is simply a couple of Age x Year (0-based)
(defstruct (state
             (:conc-name "STATE."))
  (age  0)
  (year 0))

(defparameter *age-maintenance* #(60 80 120))
(defparameter *age-resale* #(-800 -600 -500))
(defparameter *buy-cost* 1000)

(defmethod graph.vertices ((graph (eql :plan)))
  "The vertices are all the states reachable from (1, 1)."
  (transitive-closure graph (make-state)))

(defmethod vertex.arcs ((graph (eql :plan)) (vertex (eql :end)))
  "The :END pseudo-vertex has no successor"
  nil)

(defmethod vertex.arcs ((graph (eql :plan)) (vertex state))
  "Arcs are represented as CONSes of destination, arc weight."
  (declare (type state vertex))
  (let ((year (state.year vertex))
        (age  (state.age  vertex)))
    (if (= 4 year)
        (list (cons :end (+ (aref *age-maintenance* age)
                            (aref *age-resale* age))))
        (let ((arcs '()))
          (push (cons (make-state :age 0 :year (1+ year))
                      (+ (aref *age-maintenance* age)
                         *buy-cost*
                         (aref *age-resale* age)))
                arcs)
          (unless (= 2 age)
            (push (cons (make-state :age (1+ age) :year (1+ year))
                        (+ (aref *age-maintenance* age)))
                  arcs))
          arcs))))

(defmethod arc.data ((graph (eql :plan)) arc)
  (values (car arc) (cdr arc)))

(defun draw-plan ()
  (multiple-value-bind (path-length path distances)
      (shortest-path :plan (make-state) :end)
    (declare (ignore path-length))
    (graph.graphviz
     :plan
     :to-highlight path
     :label-fun (lambda (graph x)
                  (let ((dist (vertex.val graph distances x "n/a")))
                    (if (eq x :end)
                        (format nil "T [~A]" dist)
                        (let ((year (state.year x))
                              (age  (state.age  x)))
                          (format nil "(~A, ~A) [~A]"
                                  (1+ age) (1+ year) dist))))))))
