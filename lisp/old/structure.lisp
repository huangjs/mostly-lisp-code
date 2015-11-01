;;; change the way lisp print your structure.
(defstruct (ship
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (print-unreadable-object (struct stream)
                  (format stream "ship ~a of ~a at (~d, ~d) moving (~d, ~d)"
                          (ship-name struct)
                          (ship-player struct)
                          (ship-x-pos struct)
                          (ship-y-pos struct)
                          (ship-x-vel struct)
                          (ship-y-vel struct))))))
  (name "unnamed")
  player
  (x-pos 0.0)
  (y-pos 0.0)
  (x-vel 0.0)
  (y-vel 0.0))

;;; shorten slot accessor names
(defstruct (galaxy-class-cruiser-ship
             (:conc-name gcc-ship-))    ; name includes trailing hyphen!
  name player (x-pos 0.0) (y-pos 0.0) (x-vel 0.0) (y-vel 0.0))

(let ((ship (make-galaxy-class-cruiser-ship)))
  (print (gcc-ship-x-pos ship))  ; note abbrevated accessor name
  (values))

;;; without using keyword arguments
(defstruct (3d-point
             (:constructor
              create-3d-point (x y z)))
  x y z)

(create-3d-point 1 -2 3)
;;; !! note the slot values does not default to nil!
(create-3d-point 1 -2)

;;; inheritance of a structure
(defstruct employee
  name department salary social-security-number telephone)

(defstruct (manager
             (:include employee))
  bonus direct-reports)

