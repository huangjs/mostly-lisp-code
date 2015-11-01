(defpackage :empty (:use))

(defclass class-with-two-slots ()
  ((slot1 :initarg :slot1)
   (slot2 :initarg :slot2)))

(heapgraph::dump-object #p"/etc/passwd"
			"/home/david/share/pathname.dot"
			:show-fixups t
			:if-exists :supersede)

(heapgraph::dump-object (find-package :empty)
			"/home/david/share/package-empty.dot"
			:if-exists :supersede)

(heapgraph::dump-object #'print
			"/home/david/share/function-print.dot"
			:if-exists :supersede)

(heapgraph::dump-object (make-instance 'class-with-two-slots
				       :slot1 "foo"
				       :slot2 "bar")
			"/home/david/share/simple-clos-object.dot"
			:show-fixups t
			:if-exists :supersede)

(heapgraph::dump-object (make-instance 'class-with-two-slots
				       :slot1 (make-instance 'class-with-two-slots
							     :slot1 "foo"
							     :slot2 "bar")
				       :slot2 "baz")
			"/home/david/share/two-clos-objects.dot"
			:show-fixups t
			:if-exists :supersede)
