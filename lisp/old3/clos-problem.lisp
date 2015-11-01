;;; linearization
(defclass boat () ())
(defclass day-boat (boat) ())
(defclass engine-less (day-boat) ())
(defclass pedal-wheel-boat (engine-less) ())
(defclass wheel-boat (boat) ())
(defclass pedal-wheel-boat (engine-less wheel-boat) ())
(defclass small-multinull (day-boat) ())
(defclass small-catamaran (small-multinull) ())
(defclass pedalo (pedal-wheel-boat small-catamaran) ())
(sb-pcl:compute-class-precedence-list (find-class 'pedal-wheel-boat))
(sb-pcl:compute-class-precedence-list (find-class 'pedalo))

