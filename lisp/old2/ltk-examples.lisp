(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :ltk)
  (use-package :ltk-mw))

(defun make-counter (&key (initial-value 0))
  (let ((n initial-value))
	(lambda ()
	  (incf n))))

(defparameter *hello-c* (make-counter))

(defun hello ()
  (with-ltk ()
	(let* ((f (make-instance 'frame :borderwidth 3 :relief :sunken)) 
		   (text (make-instance 'text :master f :background :snow))
		   (b1 (make-instance
				'button
				:master f
				:text "Clear"
				:activebackground :red
				:activeforeground :yellow
				:command
				(lambda ()
				  (clear-text text))))
		   (b2 (make-instance
				'button
				:master f
				:text "Save"
				:command
				(lambda ()
				  (save-text text "temp.txt"))))
		   (pg (make-instance
				'progress)))
	  ;; geometrical layout
	  (pack f)
	  (pack text)
	  (pack b1) (pack b2)
	  (pack pg)
	  ;; configuration
	  (setf (text text) "Enter something here...")
	  (let ((timer (make-timer (lambda () (incf (percent pg))))))
		(unwind-protect
			 (schedule-timer timer 0.5 :repeat-interval 0.5)
		  (unschedule-timer timer))))))


(defun canvas-test ()
  (with-ltk ()
	(let* ((f (make-instance 'frame))
		   (sc (make-instance
				'scrolled-canvas
				:master f))
		   (c (canvas sc))
		   (line (create-line c (list 100 100 400 50 700 150)))
		   (polygon (create-polygon c (list 50 150 250 160 250)))
		   (text (create-text c 260 250 "Canvas Test")))
	  (pack sc :expand t :fill :both)
	  (scrollregion c 0 0 800 800))))

(defun scribble () 
  (with-ltk () 
	(let* ((canvas (make-instance
					'canvas
					:width 800
					:height 600
					:background :snow)) 
		   (down nil)) 
	  (pack canvas)
	  (bind canvas "<ButtonPress-1>" 
			(lambda (evt) 
			  (setf down t) 
			  (create-oval canvas 
						   (- (event-x evt) 10) (- (event-y evt) 10) 
						   (+ (event-x evt) 10) (+ (event-y evt) 10)))) 
	  (bind canvas "<ButtonRelease-1>" (lambda (evt) 
										 (declare (ignore evt)) 
										 (setf down nil))) 
	  (bind canvas "<Motion>" 
			(lambda (evt) 
			  (when down 
				(create-oval canvas 
							 (- (event-x evt) 10) (- (event-y evt) 10) 
							 (+ (event-x evt) 10) (+ (event-y evt) 10)))))))) 

(defun with-widgets-test ()
  (with-ltk ()
    (with-widgets
		(toplevel top-frame :title "with-widgets-test"
				  (label lb1 :text "Test, Test!" :pack '(:side :top))
				  (entry en1 :pack '(:side :top))
				  (frame fr1 :pack '(:side :bottom) 
						 (button bt1 :text "OK" :pack '(:side :right)
								 :command (lambda () (format t "Pressed OK~%")))
						 (button bt2 :text "CANCEL" :pack '(:side :left)
								 :command (lambda () (withdraw top-frame)))))
      (setf (text lb1) "Test, Test, Test!")
	  )))
