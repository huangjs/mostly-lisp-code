(in-package :clim-user)
 
(define-application-frame test
  ()
  ((height :initform 55 :accessor height))
  (:panes
   (main :application :display-function 'display-main)
   (prompter :interactor))
  (:layouts
   (:default (vertically ()
                         (2/3 (bordering () main))
                         (1/3 prompter)))))
 
(defmethod display-main ((frame test) stream)
  (let ((x 55)
        (y 55)
        (width 55))
    (draw-rectangle* stream x y (+ x width) (+ y (height frame))
                     :ink +red+)))
 
(define-test-command (com-exit :menu "Exit" :name t) ()
  (frame-exit *application-frame*))
 
(define-test-command (com-test :menu "Test" :name t)
  ((new-height 'integer :default (height *application-frame*)))
  "Changes the default value of the height slot."
  (setf (height *application-frame*) new-height))
