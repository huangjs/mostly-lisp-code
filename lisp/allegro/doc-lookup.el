(defun allegro-doc-lookup (symbol)
  (interactive (list (slime-read-symbol-name "Allegro Doc for: ")))
  (slime-eval-async
   `(ide:eval-in-listener-thread '(cl:let ((wnd (cl:make-instance 'cg:text-edit-window

								  :name :get-help 
								  :owner (cg:screen cg:*system*)
								  :state :shrunk 
								  :border :frame)))
				   (cl:format (cg:frame-child wnd) "~a" ',symbol)
				   (devel::windows-help-command wnd)
				   (cl:close wnd)))))

