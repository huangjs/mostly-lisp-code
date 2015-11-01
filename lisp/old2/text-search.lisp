;;; written 2007-03-15 and -16
;;; by Andreas Fuchs <asf@boinkor.net>
;;; MIT licence. 

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (cl:require :mcclim))

(cl:defpackage :my-favourite-algorithm.text-search
  (:use :clim-lisp :clim))

(in-package :my-favourite-algorithm.text-search)

(deftype entry-status ()
  '(member :unvisited :current :match :no-match))

(defclass entry ()
     ((char :initarg :char :accessor entry-char)))

(defclass search-entry (entry)
     ((status :initform :unvisited :type entry-status :accessor entry-status)))

;;; printing an entry
(defmethod print-object ((o entry) stream)
  (print-unreadable-object (o stream :type nil :identity nil)
    (format stream "~A" (entry-char o))))

(defmethod print-object ((o search-entry) stream)
  (print-unreadable-object (o stream :type nil :identity nil)
    (format stream "~A~A"
            (entry-char o)
            (ecase (entry-status o)
              (:unvisited "")
              (:current "^")
              (:match "_")
              (:no-match "X")))))

;;; creating an entry
(defun make-entry-string (text &key (entry-type 'entry))
  (map 'vector
       (lambda (c)
         (make-instance entry-type :char c))
       text))

;;; comparing entries
(defun entry-equalp (entry1 entry2)
  (eql (entry-char entry1)
       (entry-char entry2)))

;;; marking entries for display
(defmethod mark-entry (entry-index text entry-status)
  (setf (entry-status (aref text entry-index)) entry-status))

(defmethod mark-entries-unvisited (search-text)
  (loop for i from 0 below (length search-text)
        do (mark-entry i search-text :unvisited)))

;;; visualization status

(defclass visualization-status ()
     ((text :initarg :text :reader text)
      (body :initarg :body :reader body)
      (char-counts :initform nil :reader char-counts)
      (text-start :initform 0 :accessor text-start)
      (index-in-text :initform 0 :accessor index-in-text)))

(defmethod initialize-instance :after ((o visualization-status) &key body &allow-other-keys)
  (setf (slot-value o 'char-counts)
        (make-array (length body))))

(defun char-count (status text-start index-in-text)
  (aref (slot-value status 'char-counts) (+ text-start index-in-text)))

(defun (setf char-count) (new-value status text-start index-in-text)
  (setf (aref (slot-value status 'char-counts) (+ text-start index-in-text))
        new-value))

;;; breaking algorithms into visualizable steps
(defmacro do-visualizing-each-step ((text body text-start index-in-text) &key
                                    first-text-start next-text-start
                                    first-index-in-text next-index-in-text)
  (let ((status (gensym))
        (next-op (gensym)))
    `(let ((,status (make-instance 'visualization-status :text ,text :body ,body))
           (,next-op :initialize))
       (with-slots ((,text-start text-start) (,index-in-text index-in-text))
                   ,status
          (labels ((perform-step ()
                     ;; a state machine for nested loops. let's hope
                     ;; this works... (it does).
                     (values
                      (setf ,next-op
                            (ecase ,next-op
                              (:initialize
                               (setf text-start ,first-text-start)                        
                               :compare-start)
                              (:compare-start
                               (setf ,index-in-text ,first-index-in-text)
                               (mark-entry ,index-in-text ,text :current)
                               :compare-do)
                              (:compare-do
                               (incf (char-count ,status ,text-start ,index-in-text))
                               (cond ((entry-equalp
                                       (aref ,body (+ ,text-start ,index-in-text))
                                       (aref ,text ,index-in-text))
                                      (mark-entry ,index-in-text ,text :match)
                                      (cond
                                        ((null (position :unvisited ,text
                                                         :key #'entry-status))
                                         :end)
                                        (t :compare-step)))
                                     (t (mark-entry ,index-in-text ,text :mismatch)
                                      :reset-and-move-to-next)))
                              (:compare-step
                               (setf ,index-in-text ,next-index-in-text)
                               (mark-entry ,index-in-text ,text :current)
                               :compare-do)
                              (:reset-and-move-to-next
                               (mark-entries-unvisited ,text)
                               (psetf ,index-in-text ,first-index-in-text
                                      ,text-start ,next-text-start)
                               (cond
                                 ((> (+ (length text) ,text-start) (length body))
                                  :end)
                                 (t (mark-entry ,index-in-text ,text :current)
                                  :compare-do)))
                              (:end :end)))
                      ,status)))
            #'perform-step)))))

;;; algorithms

(defun search-for-text.naive (text body)
  (do-visualizing-each-step (text body text-start index-in-text)
   :first-text-start 0
   :next-text-start (1+ text-start)
   :first-index-in-text 0
   :next-index-in-text (1+ index-in-text)))

(defun search-for-text.naive.from-end (text body)
  (do-visualizing-each-step (text body text-start index-in-text)
   :first-text-start 0
   :next-text-start (1+ text-start)
   :first-index-in-text (1- (length text))
   :next-index-in-text (1- index-in-text)))



;;; boyer-moore skip table

(defun first-character-mismatch-skip-table (text)
  (let ((result nil))
    (loop for i from 0
          for c across (reverse text)
          do (pushnew (cons c i) result :key #'car))
    result))

(defun subpattern-fits-p (pattern text by)
  
  (let* ((patternlength (if (> by (- (length text) (length pattern)))
                            (+ (length pattern)
                               (- (length text)
                                  (length pattern)
                                  by))
                            (length pattern)))
         (start1 (if (> by (- (length text) (length pattern)))
                     (- (- (length text)
                           (length pattern)
                           by))
                     0))
         (start2 (max 0 (- (length text) (length pattern) by)))
         (end2 (+ start2 patternlength)))
    #+nil(format *debug-io* "~A = ~A/~A~%" start1 start2 end2)
    #+nil(format *debug-io* "~A=~A~%"
                 (subseq pattern start1)
                 (subseq text start2 end2))
    ;; one possible optimization: if character preceding the current
    ;; subpattern is the same as the one preceding the subpattern
    ;; inside the pattern, we should advance the subpattern further to
    ;; the left, as it has no chance to match, either. (consider
    ;; subpattern "N" of pattern "ANPANMAN".)
    (string= pattern text :start1 start1 :start2 start2 :end2 end2)))

 (defun pattern-mismatch-skip-table (text)
  (loop for subpattern-start from 1 to (1- (length text))
        for subpattern = (subseq text subpattern-start)
        collect  (cons subpattern-start
                       (loop for shift-by from 1 to (length text)
                             when (subpattern-fits-p subpattern text shift-by)
                               do (return shift-by)))))

(defun search-for-text.boyer-moore (text body)
  (let* ((first-cmt (first-character-mismatch-skip-table (map 'string 'entry-char text)))
         (subpattern-mt (pattern-mismatch-skip-table (map 'string 'entry-char text))))
    (do-visualizing-each-step (text body text-start index-in-text)
     :first-text-start 0
     :next-text-start
     (progn
       (+ text-start
          (cond ((= index-in-text (1- (length text)))
                 (or (cdr (assoc (entry-char (aref body (+ text-start index-in-text)))
                                 first-cmt))
                     (length text)))
                (t
                 (cdr (assoc (1+ index-in-text) subpattern-mt))))))
     :first-index-in-text (1- (length text))
     :next-index-in-text (1- index-in-text))))

;;; Knuth-Morris-Pratt
(defun make-restart-vector (pattern &optional (predicate #'eql) key)
  "Returns restart vector for non-empty sequence PATTERN,
suitable for string matching functions like Knuth-Morris-Pratt.
Elements of PATTERN are compared with PREDICATE, a binary test
function.

The argument to the KEY function are the elements of PATTERN.
The return value of the KEY function becomes an argument to
PREDICATE.  If KEY is not supplied or NIL, the element itself is
used.  There is no guarantee on the number of times the KEY will
be called.

Runs in O(M) time and space, with M being (length PATTERN)."
  (check-type pattern sequence "a sequence")
  (setf pattern (coerce pattern 'vector)
        key (or key #'identity))
  (let ((rv (make-sequence 'vector (length pattern))))
    (prog1 rv
      (symbol-macrolet ((cur-idx (aref rv i)))
        (loop initially (setf (aref rv 0) -1)
              for i from 1 below (length pattern) do
              (setf cur-idx (1+ (aref rv (1- i))))
              (loop
               while (and (plusp cur-idx)
                          (not (funcall predicate
                                        (funcall key (aref pattern (1- i)))
                                        (funcall key (aref pattern (1- cur-idx))))))
               do (setf cur-idx (1+ (aref rv (1- cur-idx))))))))))

(defun search-for-text.kmp (text body)
  (declare (optimize (debug 3)))
  (let ((rvec (make-restart-vector text #'eql #'entry-char)))
    (do-visualizing-each-step (text body text-start index-in-text)
      :first-text-start 0
      :next-text-start (+ text-start
                          (cond ((entry-equalp
                                  (aref text index-in-text)
                                  (aref body (+ text-start index-in-text)))
                                 0)
                                ((zerop index-in-text)
                                 1)
                                (t (- index-in-text
                                      (aref rvec index-in-text)))))
      :first-index-in-text (cond ((zerop index-in-text)
                                  0)
                                 (t (aref rvec index-in-text)))
      :next-index-in-text (cond ((< (1+ index-in-text) (length text))
                                 (1+ index-in-text))
                                (t index-in-text)))))

;;; the clim thing.

(define-application-frame visualizer ()
  ((current-closure :initform nil :accessor current-closure)
   (current-status :initform nil :accessor current-status)
   (body :initform
         (make-entry-string "the fnording lazy fox jumps over the quagmired quick brown dog.")
         :accessor current-body)
   (search-text :initform nil :accessor current-text))
  (:panes (viz :application
               :display-function 'show-progress
               ; :display-time 'command-loop
               :min-width 800)
          (instructor :application
                      :display-function 'show-help)
          (interactor :interactor
                      :max-height 200))
  (:layouts (default (vertically ()
                       (1/3 viz)
                       (1/3 instructor)
                       (1/3 interactor)))))

(defun show-progress (frame pane)
  (with-text-size (pane :large)
    (formatting-table (pane :x-spacing 1)
      (formatting-row (pane)
        (loop for c across (current-body frame)
              do (formatting-cell (pane) (princ (entry-char c) pane))))
      (if (current-text frame)
          (formatting-row (pane)
            (loop for i from 0 below (if (current-status frame)
                                         (text-start (current-status frame))
                                         0)
                  do (formatting-cell (pane)))
            (loop for c across (text (current-status frame))
                  do (formatting-cell (pane)
                       (with-drawing-options (pane :ink
                                                   (ecase (entry-status c)
                                                     (:current +orange+)
                                                     (:unvisited +black+)
                                                     (:match  +green+)
                                                     (:mismatch +red+)))
                         (princ (entry-char c) pane)))))
          (formatting-row (pane)
            (mapc (lambda (c)
                    (formatting-cell (pane)
                      (princ c pane)))
                  (coerce "NO SEARCH TERM" 'list))))
      (when (current-status frame)
        (formatting-row (pane)
          (loop for count across (char-counts (current-status frame))
                sum count into all-counts
                do (formatting-cell (pane)
                     (unless (zerop count)
                       (format pane "~A" count)))                  
                finally (formatting-cell (pane)
                          (format pane " = ~A" all-counts))))))))

(defun show-help (frame pane)
  (with-text-family (pane :serif)
    (format pane "To enter a ~@[new ~]search term, use " (current-text frame))
    (present 'set-search-text 'command-name :stream pane)
    (format pane ". Example usage (click):~%")
    
    (indenting-output (pane 10)
      (with-output-as-presentation (pane '(set-body "the fnording lazy fox jumps over the quagmired quick brown dog.") 'command)
        (format pane "Set Body (\"The...Fox\")"))
      (terpri pane)
      (indenting-output (pane 20)
        (with-output-as-presentation (pane '(set-search-text "quick brown" :boyer-moore) 'command)
          (format pane "Set Search Text (\"quick brown\", Boyer-Moore)"))
        (terpri pane)
        (with-output-as-presentation (pane '(set-search-text "fox" :naive) 'command)
          (format pane "Set Search Text (\"fox\", Naive)")))
      (terpri pane))

    (indenting-output (pane 10)
      (with-output-as-presentation (pane '(set-body "barabarabarabarabarabas") 'command)
        (format pane "Set Body (\"barabarabarabarabarabas\")"))
      (terpri pane)
      (indenting-output (pane 20)
        (with-output-as-presentation (pane '(set-search-text "barabas" :boyer-moore) 'command)
          (format pane "Set Search Text (\"barabas\", Boyer-Moore)"))
        (terpri pane)
        (with-output-as-presentation (pane '(set-search-text "barabas" :knuth-morris-pratt)
                                           'command)
          (format pane "Set Search Text (\"barabas\", Knuth-Morris-Pratt)")))
      (terpri pane))

    (when (current-text frame)
      (format pane "~&To advance the search one step, use ")
      (present '(perform-step) 'command :stream pane)
      (format pane " (bound to C-RET).~%"))))

(define-visualizer-command (set-body :name t) ((body 'string))
  (setf (current-body *application-frame*) (make-entry-string body)
        (current-text *application-frame*) nil
        (current-status *application-frame*) nil))

(define-visualizer-command (set-search-text :name t)
    ((search-text 'string)
     (algorithm '(member :naive :naive-from-end
	                 :boyer-moore :knuth-morris-pratt)
                :default :boyer-moore
                :name "Search algoritm"))
  (setf (current-text *application-frame*)
        (make-entry-string search-text :entry-type 'search-entry)
        
        (current-closure *application-frame*)
        (funcall
         (ecase algorithm
           (:naive 'search-for-text.naive)
           (:naive-from-end 'search-for-text.naive.from-end)
           (:boyer-moore 'search-for-text.boyer-moore)
           (:knuth-morris-pratt 'search-for-text.kmp))
         (current-text *application-frame*) 
                                     (current-body *application-frame*)))
  (perform-step))

(define-visualizer-command (perform-step :name "Step" :keystroke (#\Return :control)) ()
  (multiple-value-bind (step status) (funcall (current-closure *application-frame*))
    (declare (ignore step))
    (setf (current-status *application-frame*) status)))