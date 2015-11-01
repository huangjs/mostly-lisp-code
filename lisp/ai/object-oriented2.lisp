;;; domain specifiable searching tools
(defclass problem ()
  ((state :initarg :states :accessor problem-states)))

;;; searcher does not assume that the problem states are
;;; organized in a list.
(defmethod searcher ((prob problem))
  "Find a state that solves the search problem."
  (cond ((no-states-p prob) fail)
        ((goal-p prob) (current-state prob))
        (t (let ((current (pop-state prob)))
             (setf (problem-states prob)
                   (problem-combiner
                    prob
                    (problem-successors prob current)
                    (problem-states prob))))
           (searcher prob))))

;;; for the basic problem class,
;;; we will in fact implement the states as a list.
(defmethod current-state ((prob problem))
  "The current state is the first of the possible states."
  (first (problem-states prob)))

(defmethod pop-state ((prob problem))
  "Remove and return the current state."
  (pop (problem-states prob)))

(defmethod no-states-p ((prob problem))
  "Are there any more unexplored states?"
  (null (problem-states prob)))

(defmethod searcher :before ((prob problem))
  (dbg 'search "~&;; Search: ~a" (problem-states prob)))

;;; defining goal
(defclass eql-problem (problem)
  ((goal :initarg :goal :reader problem-goal)))

(defmethod goal-p ((prob eql-problem))
  (eql (current-state prob) (problem-goal prob)))

;;; specify two search strategies
(defclass dfs-problem (problem) ()
          (:documentation "Depth-first search problem."))

(defclass bfs-problem (problem) ()
          (:documentation "Breadth-first search problem."))

(defmethod problem-combiner ((prob dfs-problem) new old)
  "Depth-first search looks at new states first."
  (append new old))

(defmethod problem-combiner ((prob bfs-problem) new old)
  "Breadth-first search looks at old states first."
  (append old new))


;;; a particular problem domain
(defclass binary-tree-problem (problem) ())

(defmethod problem-successors ((prob binary-tree-problem) state)
  (let ((n (* 2 state)))
    (list n (+ n 1))))

;;; define a solver on binary-tree problem
(defclass bfs-binary-tree-solver
    (binary-tree-problem eql-problem bfs-problem) ())

(deftest test-solver ()
  (show
    (setf p1 (make-instance 'bfs-binary-tree-solver
                            :states '(1) :goal 12))
    (searcher p1)
    ))


;;; best first search
(defclass best-problem (problem) ()
          (:documentation "A Best-first search problem."))

(defmethod problem-combiner ((prob best-problem) new old)
  "Best-first search sorts new and old according to cost-fn."
  (sort (append new old) #'<
        :key #'(lambda (state) (cost-fn prob state))))

;;; example of cost-fn
;;; for any eql-problem dealing with numbers.
(defmethod cost-fn ((prob eql-problem) state)
  (abs (- state (problem-goal prob))))


;;; beam search
(defclass beam-problem (problem)
  ((beam-width :initarg :beam-width :initform nil
               :reader problem-beam-width)))

(defmethod problem-combiner :around ((prob beam-problem) new old)
  (let ((combined (call-next-method)))
    (subseq combined 0 (min (problem-beam-width prob)
                            (length combined)))))

;;; define a best-beam search based solver
;;; on binary-tree problem ........ very long name
(defclass best-beam-binary-tree-solver
    (binary-tree-problem eql-problem best-problem beam-problem)
  ())

(deftest test-beam-solver ()
  (show
    (setf p3 (make-instance 'best-beam-binary-tree-solver
                            :states '(1) :goal 12 :beam-width 3))
    (searcher p3)
    ))


;;; Now we start to implement the trip planner
;;; adop the searcher class into a particular domain

;;; (load search.lisp) first

(defclass trip-planner (best-beam-binary-tree-solver)
  ((beam-width :initform 1)))

;;; method override
(defmethod cost-fn ((prob trip-planner) city)
  (air-distance (problem-goal prob) city))

(defmethod problem-successors ((prob trip-planner) city)
  (neighbors city))

(deftest test-trip-planner ()
  (show
    (setf p4 (make-instance 'trip-planner
                            :states (list (city 'New-York))
                            :goal (city 'San-Francisco)))
    (searcher p4)
    ))

