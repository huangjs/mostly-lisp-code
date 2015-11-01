(eval-when (:compile-toplevel :load-toplevel :execute)
  (gtk:clg-init-with-threading))

(defmacro gtk (&body body)
  `(gtk:within-main-loop ,@body))

(defmacro gset (name &body body)
  `(setf ,name (gtk ,@body)))

(defun foo ()
  (gtk:within-main-loop
    (let* ((label
	    (make-instance 'gtk:label
			   :label "<b><i>Jianshi</i></b>"
			   :use-markup t))
	   (button1
	    (make-instance 'gtk:button
			   :label "RESIZE"
			   :relief :normal
			   :signal (list
				    'clicked (lambda ()
					       (print "button clicked.")))))
	   (button2
	    (make-instance 'gtk:button
			   :label "ME"))
	   (hbox
	    (make-instance 'gtk:h-box
			   :child button1
			   :child button2))
	   (handle-box
	    (make-instance 'gtk:handle-box
			   :shadow-type :in
			   :handle-position :left
			   :snap-edge :top
			   :child hbox))
	   (window
	    (make-instance 'gtk:window
			   :title "Huang"
			   :border-width 10
			   :default-height 100
			   :default-width 200
			   :child handle-box
			   :signal (list 
				    'delete-event (lambda (event)
						    (declare (ignore event))
						    (write-line "Destroying window")
						    nil ; Returning NIL generates a destroy event
						    )))))
      (gtk:widget-show-all window)
      (list
       window
       ;;(gtk:signal-connect window 'destroy (lambda (event) (print "Window closed.")))
       ))))

