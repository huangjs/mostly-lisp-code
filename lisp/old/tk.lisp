(in-package :ltk-user)

;;; example 1
(defun hello-1()
  (ltk:with-ltk ()
    (let ((b (make-instance 'ltk:button
                            :master nil
                            :text "Press Me"
                            :command (lambda ()
                                       (format t "Hello World!~&")))))
      (ltk:pack b))))


;;; example 2
(defun hello-2()
  (with-ltk ()
	(let* ((f (make-instance 'frame))
		   (b1 (make-instance 'button
							  :master f
							  :text "Button 1"
							  :command (lambda () (format t "Button1~&"))))
		   (b2 (make-instance 'button
							  :master f
							  :text "Button 2"
							  :command (lambda () (format t "Button2~&"))))
		   (c1 (make-instance 'check-button
							  :master f
							  :text "Check Button"
							  :command (lambda (value)
										 (format t "Check button: ~a~&" value)))))
	  (pack f :expand t
			:fill :both) 
	  (pack b1 :side :left) 
	  (pack b2 :side :left)
	  (pack c1 :side :left)
	  (configure f :borderwidth 3)
	  (configure f :relief :sunken)
	  )))



;;; example: bind
(defun scribble ()
  (with-ltk ()
	(let* ((canvas (make-instance 'canvas))
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


;;; example: hello world
(defun hello-world ()
  (setf *debug-tk* nil)
  (with-ltk ()
    (let ((b (make-instance
              'button
              :text "Hello World!"
              :command (lambda ()
                         (do-msg "Bye!" "Hello World!")
                         (setf *exit-mainloop* t)))))
      (pack b))))

;;; example: all widgets
(defun all-widgets ()
  (with-ltk ()
	(let ((la (make-instance 'label
							 :text "Label"))
		  (bu (make-instance 'button
							 :text "Button"
							 :command (lambda ()
										(do-msg "Bye!" "Hello World!")
										(setf *exit-mainloop* t))))
		  (ch (make-instance 'check-button
							 :text "Check Button"))
		  (ra (make-instance 'radio-button
							 :text "Radio Button"))
		  (sc (make-instance 'scale
							 :label "Scale"
							 :to 100
							 :length 100))
		  (en (make-instance 'entry
							 :text "Entry"))
		  (me (make-instance 'menu
							 :text "Menu"))
		  (mb (make-instance 'menubutton
							 :text "Menu Button"))
		  (li (make-instance 'listbox	;error
							 :text "Listbox"))
		  (sr (make-instance 'scrollbar
							 :text "Scrollbar")))
	  (pack la)
	  (pack bu)
	  (pack ch)
	  (pack ra)
	  (pack sc)
	  (pack en)
	  (pack li)
	  (pack sr))))


;;;example: three buttons
(defun three-buttons ()
  (with-ltk ()
	(let ((first (make-instance 'button
								:text "One"
								:background :blue
								:foreground :white))
		  (second (make-instance 'button
								 :text "Second"
								 :background :red
								 :foreground :white))
		  (third (make-instance 'button
								:text (format nil "Third~&Button")
								:background :beige
								:foreground :purple)))
	  (pack (list first second third)
			:side :left))))

;;; example: official ltktest
(defvar *do-rotate* nil)
(defvar *demo-line* nil)
(defvar *demo-canvas* nil)

(defun eggs (radio)
  (format t "Prepare ~a eggs.~%"
          (case (value radio)
            (1 "fried")
            (2 "stirred")
            (3 "cooked")))
  (finish-output))

(defun ltktest()
  (with-ltk ()
	(let* ((bar (make-instance 'frame))
		   (fradio (make-instance 'frame :master bar))
		   (leggs (make-instance 'label :master fradio :text "Eggs:"))
		   (r1 (make-instance 'radio-button :master fradio :text "fried" :value 1 :variable "eggs"))
		   (r2 (make-instance 'radio-button :master fradio :text "stirred" :value 2 :variable "eggs"))
		   (r3 (make-instance 'radio-button :master fradio :text "cooked" :value 3 :variable "eggs"))
		   (fr (make-instance 'frame :master bar))
		   (lr (make-instance 'label :master fr :text "Rotation:"))
		   (bstart (make-instance 'button :master fr :text "Start" :command 'start-rotation))
		   (bstop  (make-instance 'button :master fr :text "Stop"  :command 'stop-rotation))
		   (b1 (make-instance 'button :master bar :text "Hallo"
							  :command (lambda ()
										 (format t "Hallo~%")
										 (finish-output))))
		   (b2 (make-instance 'button :master bar :text  "Welt!"
							  :command (lambda ()
										 (format t "Welt~%")
										 (finish-output))))
		   (f (make-instance 'frame :master bar))
		   (l (make-instance 'label :master f :text "Test:"))
		   (b3 (make-instance 'button :master f :text  "Ok." :command 'test-rotation))
		   (e (make-instance 'entry :master bar))
		   (b4 (make-instance 'button :master bar :text "get!"
							  :command (lambda ()
										 (format t "content of entry:~A~%" (text e))
										 (finish-output))))
		   (b5 (make-instance 'button :master bar :text "set!"
							  :command (lambda ()
										 (setf (text e) "test of set"))))
		   (sc (make-instance 'scrolled-canvas :borderwidth 2 :relief :raised))
		   (c (canvas sc))
		   (lines nil)
		   (mb (make-menubar))
		   (mfile (make-menu mb "File" ))
		   (mf-load (make-menubutton mfile "Load" (lambda () ;(error "asdf")
													(format t "Load pressed~&")
													(finish-output))
									 :underline 1))
		   (mf-save (make-menubutton mfile "Save" (lambda ()
													(format t "Save pressed~&")
													(finish-output))
									 :underline 1))
		   (sep1 (add-separator mfile))
		   (mf-export (make-menu mfile "Export..."))
		   (sep2 (add-separator mfile))
		   (mf-print (make-menubutton mfile "Print" (lambda () (postscript c "wt.ps"))))
		   (sep3 (add-separator mfile))
		   (mfe-jpg (make-menubutton mf-export "jpeg" (lambda ()
														(format t "Jpeg pressed~&")
														(finish-output))))
		   (mfe-gif (make-menubutton mf-export "png" (lambda ()
													   (format t "Png pressed~&")
													   (finish-output))))

		   (mf-scale (make-menu mfile "Scale..."))
		   (mfs-1 (make-menubutton mf-scale "0.5" (lambda ()
													(scale c 0.5))))
		   (mfs-2 (make-menubutton mf-scale "2" (lambda ()
												  (scale c 2))))
		   (mfs-3 (make-menubutton mf-scale "2/0.5" (lambda ()
													  (scale c 2 0.5))))
		   (mfs-4 (make-menubutton mf-scale "0.5/2" (lambda ()
													  (scale c 0.5 2))))
		   (sep4 (add-separator mfile))
		   (mf-exit (make-menubutton mfile "Exit" (lambda () (setf *exit-mainloop* t))
									 :underline 1
									 :accelerator "Alt Q"))
		   (mp (make-menu nil "Popup"))
		   (mp-1 (make-menubutton mp "Option 1" (lambda () (format t "Popup 1~&") (finish-output))))
		   (mp-2 (make-menubutton mp "Option 2" (lambda () (format t "Popup 2~&") (finish-output))))
		   (mp-3 (make-menubutton mp "Option 3" (lambda () (format t "Popup 3~&") (finish-output))))
		   )
	  (declare (ignore mf-print mf-exit mfe-gif mfe-jpg mf-save mf-load sep1 sep2 sep3 sep4 mp-1 mp-2 mp-3 mfs-1 mfs-2 mfs-3 mfs-4)) 


	

	  (bind *tk* "<Alt-q>" (lambda (event) (declare (ignore event)) (setf *exit-mainloop* t)))

	  (bind c "<1>" (lambda (event) (popup mp (event-root-x event) (event-root-y event))))
	  (configure c :borderwidth 2 :relief :sunken)
	  (pack sc :side :top :fill :both :expand t)
	  (pack bar :side :bottom)
	  (pack (list fradio leggs r1 r2 r3) :side :left)
	  (dolist (r (list r1 r2 r3))
		(let ((button r))
		  (setf (command r) (lambda (val)
							  (declare (ignore val))
							  (eggs button)))))
	  (scrollregion c 0 0 500 400)
	  (pack fr :side :left)
	  (pack lr :side :left)
	  (configure fr :borderwidth 2 :relief :sunken)
	  (pack bstart :side :left)
	  (pack bstop :side :left)
	  (pack b1 :side :left)
	  (pack b2 :side :left)
	  (configure f :borderwidth 2 :relief :sunken)
	  (pack f :fill :x :side :left)
	  (pack l :side :left)
	  (pack b3 :side :left)
	  (pack e :side :left)
	  (pack b4 :side :left)
	  (pack b5 :side :left)
	  (dotimes (i 100)
		(let ((w (* i 2.8001f0)))
		  (let ((x (+ 250 (* 150.0f0 (sin w))))
				(y (+ 200 (* 150.0f0 (cos w)))))
			(push y lines)
			(push x lines)
			)))
	  (setf *demo-line* (create-line c lines))
	  (setf *demo-canvas* c)
	  (create-text c 10 10 "Ltk Demonstration")
	  )))

(defvar *angle* 0.0f0)
(defvar *angle2* 0.0f0)
(defvar *angle3* 0.0f0)
(declaim (single-float *angle* *angle2* *angle3*))

(defun rotate()
;  (declare (optimize speed)    (single-float *angle* *angle2* *angle3*))
  (let ((*debug-tk* nil))
    (let ((lines nil)
	  (dx (* 50 (sin *angle2*)))
	  (dy (* 50 (cos *angle2*)))
	  (wx (sin *angle3*))
;	  (wy (cos *angle3*))
	  )
      (incf *angle* 0.1f0)
      (incf *angle2* 0.03f0)
      (incf *angle3* 0.01f0)
      
      (dotimes (i 100)
        (declare (fixnum i))
	(let ((w (+ *angle* (* i 2.8001f0))))
	  (let ((x (+ dx 250 (* 150 (sin w) wx)))
		(y (+ dy 200 (* 150 (cos w)))))
	    (push y lines)
	    (push x lines)
	    )))    
      (set-coords *demo-canvas* *demo-line* lines))
    (if *do-rotate*
	(after 25 #'rotate))))

(defun test-rotation()
  (setf *debug-tk* nil)
  (time (dotimes (i 1000)
	  (rotate)))
  (finish-output))

(defun start-rotation()
  (setf *do-rotate* t)
  (rotate))

(defun stop-rotation()
  (setf *do-rotate* nil))


;;;; the eyes :)

(defun ltk-eyes ()
  (with-ltk ()
   (let* ((*debug-tk* nil)
	  (w (screen-width))
	  (h (screen-height))
	  (c (make-instance 'canvas :width 400 :height 300))
	  (e1 (create-oval c 10 10 190 290))
	  (e2 (create-oval c 210 10 390 290))
	  (p1 (create-oval c 10 10 40 40))
	  (p2 (create-oval c 10 10 40 40))
	  (old-x 0)
	  (old-y 0))
     (setf *debug-tk* nil)
     (labels ((update ()
		      (multiple-value-bind (pos-x pos-y) (screen-mouse)
			(let* ((wx (window-x *tk*))
			       (wy (window-y *tk*))
			       (width (window-width *tk*))
			       (height (window-height *tk*))
			       (mx pos-x)
			       (my pos-y)
			       (x (truncate (* width (/ mx w))))
			       (y (truncate (* height (/ my h))))
			       (diam (truncate width 8))
			       (dx1 (- mx (+ wx (truncate width 4))))
			       (dy1 (- my (+ wy (truncate height 2))))
			       (dx2 (- mx (+ wx (* 3 (truncate width 4)))))
			       (dy2 (- my (+ wy (truncate height 2))))
			       (p1x (+ (- (truncate width 4)  (truncate diam 2)) (truncate (* width  dx1) (* 4.5 w))))
			       (p1y (+ (- (truncate height 2) (truncate diam 2)) (truncate (* height dy1) (* 2.3 h))))
			       (p2x (+ (- (* 3 (truncate width 4))  (truncate diam 2)) (truncate (*  width  dx2) (* 4.5 w))))
			       (p2y (+ (- (truncate height 2) (truncate diam 2)) (truncate (* height dy2) (* 2.3 h))))
			       
			       )
			  (setf *debug-tk* nil)
			  (unless (and (= x old-x)
				       (= y old-y))
			    (set-coords c e1 (list 10 10 (- (truncate width 2) 10) (- height 10)))
			    (set-coords c e2 (list (+ (truncate width 2) 10) 10  (- width 10) (- height 10)))
			    (set-coords c p1 (list p1x p1y (+ diam p1x) (+ diam p1y)))
			    (set-coords c p2 (list p2x p2y (+ diam p2x) (+ diam p2y)))
			    (setf old-x x
				  old-y y))
			  ))
	 	        (after 100 #'update)))
     (pack c :expand 1 :fill :both)
     (itemconfigure c e1 "width" 10)
     (itemconfigure c e2 "width" 10)
     (itemconfigure c p1 "fill" "blue")
     (itemconfigure c p2 "fill" "blue")
     (after 100 #'update)
     ))))


