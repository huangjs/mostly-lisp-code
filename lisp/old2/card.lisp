;; -*- Mode: Lisp; Package: POKER.NESS -*-
;; Author: <rydis@CD.Chalmers.SE>
;; Based on <URL: http://mywebpages.comcast.net/dness/poker.htm >
;; Bugs:
;;   Decent speed/memory consumption, but could be better.
;;   Declarations are probably not sufficiently exact everywhere
;;     (array-index, for instance), and probably at least in one place not
;;     quite correct.
;;   Function names sort of suck.
;;   I don't fill with 12-; I don't really see the point in doing that.

(defpackage :poker.ness (:use :CL))
(in-package :poker.ness)

(declaim (optimize (speed 3) (safety 0) #+cmu(ext:inhibit-warnings 0)))	;(debug 0)))

;;(declaim (notinline hand-cv-2))

(deftype card         () '(integer 0 51))
(deftype denomination () '(integer 0 12))
(deftype suit         () '(integer 0 3))
(deftype hand         () '(integer 0 4826808))
(deftype hand-v       () '(simple-array card (5)))
(deftype hand-type    () '(integer 0 9))
(deftype tcount       () '(unsigned-byte 8))
(deftype tcd          () '(simple-array tcount (13)))
(deftype cv-dr-v      () '(simple-array tcount (6)))
(deftype array-index  () `(integer 0 ,array-dimension-limit))

(declaim (inline card denomination suit))

(defun card (denomination suit)
  "Given a denomination (0-12) and a suit (0-3), return the encoding for that card."
  (declare (ftype card card)
		   (type denomination denomination)
		   (type suit suit))
  (+ denomination (* 13 suit)))

(defun denomination (card)
  "Given an encoded card (0-51), return its denomination (0-12)."
  (declare (ftype denomination denomination)
		   (type card card))
  (mod card 13))

(defun suit (card)
  "Given an encoded card (0-51), return its suit (0-3)."
  (declare (ftype suit suit)
		   (type card card))
  (nth-value 0 (floor card 13)))

(declaim (inline type-count-denom))

(defun type-count-denom (hand-v)
  "Given a HAND-V, return a TCD (array of 13 elements), enumerating how many of each
denomination in the hand."
  (declare (type hand-v hand-v)
		   (ftype tcd type-count-denom))
  (loop with tc-d = (make-array 13 :element-type 'tcount :initial-element 0)
	 for card across (the hand-v hand-v)
	 do (incf (aref tc-d (denomination card)))
	 finally (return tc-d)))

(defun flushp (hand-v)
  "Returns true if HAND-V represents a flush."
  (declare (type hand-v hand-v))
  (let ((target (suit (aref hand-v 0))))
    (every (lambda (x)
			 (= target (suit x)))
		   hand-v)))

;; This was the main time hog. Poorly named.
(defun t-of-tcd (hand-v)
  "Returns two values; a CV-DR and a TCD; internal"
  (declare (type hand-v hand-v)
		   (values cv-dr-v tcd)
		   (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0)))
  (let* ((tcd (type-count-denom hand-v))
		 (cv-dr (make-array 6 :element-type 'tcount)))
    (loop for i of-type (integer 0 6) from 1 to 5
	   do (setf (aref cv-dr i)
				(loop for a downfrom (1- (length tcd)) to 0
				   with max-so-far = 0
				   with max-index  = 0
				   when (> (aref tcd a) max-so-far)
				   do (setf max-so-far (aref tcd a))
				   (setf max-index a)
				   finally (setf (aref tcd max-index) 0) 
				   (return max-index))))
    (values cv-dr (type-count-denom hand-v))))

(defun map-combinations (items n fun)
  "Takes a simple-array of ITEMS, and maps FUN over all combinations of N elements.
Returns no useful values."
  (declare (type (simple-array card (*)) items)
		   (type array-index n)
		   (type function fun))
  (let ((selected  (make-array n :element-type 'card :fill-pointer 0)))
    (labels ((map-comb (items n fun sel)
			   (declare (type (vector card) items sel)
						(type array-index n)
						(type function fun))
			   (if (zerop n)
				   (funcall fun (copy-seq sel))	;Make it simple (no FP).
				   (loop for item from 0 to (1- (length items))
					  for rest = (subseq items (1+ item))
					  do (vector-push (aref items item) sel)
					  (map-comb rest (1- n) fun sel)
					  (vector-pop sel)))))
      (map-comb items n fun selected))))


(defun straightp (tcd)
  "Returns true when the TCD represents a straight."
  (declare (type tcd tcd))
  ;; LOOP declares stuff to (or (member nil) <what I want>). :-/
  (loop for a of-type tcount across (the tcd tcd)
	 for idx of-type (integer 0 13) from 0 to 12
	 with consecs of-type tcount = 0
	 when (>= a 1) do (incf consecs) 
	 when (= consecs 5) return t
	 when (and (= consecs 4) (= idx 3) (= (the tcount (aref tcd 12)) 1)) return t
	 when (= a 0) do (setf consecs 0)))

(defun hand-cv-2 (hand-v)
  "Returns the characteristic vector of a hand."
  (declare (type hand-v hand-v))
  (multiple-value-bind (cv-dr tcd)
	  (t-of-tcd hand-v)
    (declare (type cv-dr-v cv-dr) (type tcd tcd))
    (let ((straightp (straightp tcd))
		  (flushp (flushp hand-v))
		  (main (aref tcd (aref cv-dr 1)))
		  (aux  (aref tcd (aref cv-dr 2))))
      (declare (type tcount main aux))
      (let ((t-n (case main
				   (1 0)
				   (2 (if (= 2 aux) 2 1))
				   (3 (if (= 2 aux) 6 3))
				   (4 7)
				   (5 9)))
			(t-f (cond ((and straightp flushp) 8)
					   (flushp 5)
					   (straightp 4)
					   (t 0))))
		(case (setf (aref cv-dr 0) (max t-n t-f))
		  ;; Straight; re-order if 5-high.
		  ((4 8) (maybe-adjust cv-dr)))))
    cv-dr))

;; Re-order a 5-hi straight for comparisons.
(defun maybe-adjust (cv-dr)
  (declare (type cv-dr-v cv-dr))
  (when (and (= (aref cv-dr 1) 12)
			 (= (aref cv-dr 2) 3))
    (rotatef (aref cv-dr 1)
			 (aref cv-dr 2)
			 (aref cv-dr 3)
			 (aref cv-dr 4)
			 (aref cv-dr 5))))

;;; These two functions are a bit ... old-fashioned.
;; Returns a numeric representation of a hands CV.
(defun hand-cv-2->number (cv)
  (declare (type cv-dr-v cv))
  (prog ((index 5) (multiplier 1) (result 0))
	 (declare (type (integer 0 5) index)
			  (type (integer 1 #.(expt 13 5)) multiplier)
			  (type hand result))
     loop
	 (incf result (* multiplier (aref cv index)))
	 (when (> index 0)
	   (decf index)
	   (setf multiplier (* 13 multiplier))
	   (go loop))
	 (return result)))

;; Returns a CV from its numeric representation.
(defun number->hand-cv-2 (num)
  (declare (type hand num))
  (prog ((index 0) (multiplier (expt 13 5)) (hand (make-array 6 :element-type 'tcount)))
	 (declare (type (integer 0 5) index)
			  (type (integer 1 #.(expt 13 5)) multiplier))
     loop
	 (decf num (* multiplier (setf (aref hand index) (floor num multiplier))))
	 (when (< index 5)
	   (setf multiplier (floor multiplier 13)
			 index (1+ index))
	   (go loop))
	 (return hand)))


;; Best possible 5-card hand from a number of cards
(defun best-hand (cards)
  (declare (type (simple-array card (*)) cards))
  (let ((bh 0))
    (declare (type hand bh))
    (map-combinations cards 5 (lambda (hand)
								(let ((ch (hand-cv-2->number (hand-cv-2 hand))))
								  (declare (type hand ch))
								  (when (> ch bh)
									(setf bh ch)))))
	bh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OOF - End of actual code.
;;; Here comes various generations of test code
#+test
(eval-when (compile)
  (load "mrsrc:shuffle"))
#+test
(defun deck ()
  (let ((cards (make-array 52 :element-type 'card :fill-pointer 0)))
    (dotimes (i 52)
      (vector-push i cards))
    (cl-user::shuffle cards)))
#+test
(defun random-hand (&optional (n 5))
  (declare (type card n))
  (let ((cards (make-array n :element-type 'card))
		(deck (deck)))
    (loop for a from 0 below n do (setf (aref cards a) (vector-pop deck)))
    cards))

#+mrtest
(defun test-b&f (hand)
  (let* ((cv (hand-cv-2 hand))
		 (num (hand-cv-2->number cv))
		 (cv2 (number->hand-cv-2 num)))
    (unless (equalp cv2 cv)
      (princ cv2) (terpri) (princ cv)
      (break))))


#+mrtest
(defun card->string (card)
  (declare (type card card))
  (let ((val (case (denomination card)
			   (12 "A")
			   (11 "K")
			   (10 "Q")
			   (9  "J")
			   (8  "T")
			   (t (+ 2 (denomination card)))))
		(suit (case (suit card)
				(0 "s")
				(1 "h")
				(2 "d")
				(3 "c"))))
    (format nil "~A~A" val suit)))

#+mrtest
(defun print-hand (hand-v fun)
  (declare (type hand-v hand-v))
  (format t "~&[~{~A~^, ~}] (~A)"
		  (map 'list #'card->string hand-v) 
		  (print-hand-val hand-v fun)))

#+mrtest
(defun print-hand-val (hand-v fun)
  (declare (type hand-v hand-v)
		   (function fun))
  (let ((cv (funcall fun hand-v)))
    (case (aref cv 0)
      (9 "five of a kind")
      (8 "straight flush")
      (7 "four of a kind")
      (6 "full house")
      (5 "flush")
      (4 "straight")
      (3 "three of a kind")
      (2 "two pair")
      (1 "one pair")
      (0 "high card"))))


#+junk
(defun best-hand (cards)
  (let ((res (list 0)))
    (map-combinations cards 5 (lambda (x) 
								(let* ((cv (hand-cv-2 x))
									   (cvn (hand-cv-2->number cv)))
								  (when (> cvn (car res))
									(setf res (cons cvn cv))))))
    res))
  
#+mrtest-2
(defun random-tester ()
  (declaim (optimize (debug 3) (speed 0) (safety 3)))
  (loop while (string= (read-line) "")
	 for hand = (subseq (shuffle (deck)) 0 5)
	 do (princ hand) (terpri)
	 (princ (multiple-value-list (t-of-tcd hand))) (terpri)
	 ;;(print-hand hand #'hand-cv) (princ (hand-cv hand))
	 (print-hand hand #'hand-cv-2) (princ (hand-cv-2 hand))
	 (format t "~&------------------------------~%")))

;; vector-greaterp and hand-cv->number not currently used
#|
Not currently used, anyway. And a bit ugly.
(defun vector-greaterp (hand-1 hand-2)
  (declare (type hand-v hand-1 hand-2))
  (loop for h1 of-type card across hand-1
	 for h2 of-type card across hand-2
	 when (> h1 h2) return t
	 else if (< h1 h2) return nil))
|#
#|
;; Can't use until I fixup hand-cv, anyway.
(defun hand-cv->number (cv)
  (declare (type cv-dr-v cv)
		   (values hand))
  (loop for a downfrom 5 to 0
	 for multiplier = 1 then (* 13 multiplier)
	 summing (* (aref cv a) multiplier)))

(defun hand-cv-d->number (cv)
  (declare (type cv-dr-v cv)
		   (values hand))
  (loop for a of-type (integer -1 5) downfrom 5 to 0
	 for multiplier of-type (integer 0 #.(expt 13 5)) = 1 then (* 13 multiplier)
	 summing (* (aref cv a) multiplier)))

(defun number->hand-cv (num)
  (declare (type hand num))
  (let ((hand (make-array 6 :element-type 'tcount)))
    (loop for a from 0 to 5
	   for multiplier of-type hand = (expt 13 5) then (floor multiplier 13)
	   for elt = (floor num multiplier)
	   do (setf (aref hand a) elt
				num (- num (* elt multiplier))))
    hand))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Test function
#+mrtest0
(defun deck ()
  (let ((cards (make-array 52 :element-type 'card :fill-pointer 0)))
    (dotimes (i 52)
      (vector-push i cards))
    (copy-seq cards)))

#+mrtest0
(defun mrch (&optional (hand (deck)))
  (let ((highcard 0)
        (pair 0)
        (two-pair 0)
        (trips 0)
        (straight 0)
        (flush 0)
        (full-house 0)
        (quads 0)
        (straight-flush 0)
	(fives 0))
    (declare (fixnum highcard pair two-pair trips straight flush full-house quads
		     straight-flush fives))
    (map-combinations hand 5
                      #'(lambda (hand)
			  (declare (type hand-v hand))
			  (case (aref (hand-cv-2 hand) 0)
			    (0 (incf highcard))
                            (1 (incf pair))
                            (2 (incf two-pair))
                            (3 (incf trips))
                            (4 (incf straight))
                            (5 (incf flush))
                            (6 (incf full-house))
                            (7 (incf quads))
                            (8 (incf straight-flush))
			    (9 (incf fives)))))


    (values highcard
            pair
            two-pair
            trips
            straight
            flush
            full-house
            quads
            straight-flush
	    fives)))

#+mrtest0
(defun mrch0 (&optional (hand (deck)))
  (let ((highcard 0)
        (pair 0)
        (two-pair 0)
        (trips 0)
        (straight 0)
        (flush 0)
        (full-house 0)
        (quads 0)
        (straight-flush 0)
	(fives 0))
    (declare (fixnum highcard pair two-pair trips straight flush full-house quads
		     straight-flush fives))
    (map-combinations hand 5
                      #'(lambda (hand)
			  (declare (type hand-v hand))
			  (case (aref hand 0)
			    (0 (incf highcard))
                            (1 (incf pair))
                            (2 (incf two-pair))
                            (3 (incf trips))
                            (4 (incf straight))
                            (5 (incf flush))
                            (6 (incf full-house))
                            (7 (incf quads))
                            (8 (incf straight-flush))
			    (9 (incf fives)))))


    (values highcard
            pair
            two-pair
            trips
            straight
            flush
            full-house
            quads
            straight-flush
	    fives)))

#|
(defun canonicalise-hand-v (hand-v)
  (declare (values hand-v)
		   (type hand-v hand-v))
  (sort (copy-seq hand-v) #'<))

(defun reduce-hand-v (hand-v)
  (declare (values hand-v)
		   (type hand-v hand-v))
  (let ((h (canonicalise-hand-v hand-v))
		(ch (make-array 5 :element-type 'card))
		(last -1))
    (declare (type (or (integer -1 -1) card) last))
    (map-into ch (lambda (x) 
				   (declare (type card x)) 
				   (prog1 (- x (1+ last)) 
					 (setf last x)))
			  h)))

(defun denom-h (hand-v)
  (declare (type hand-v hand-v))
  (map-into (make-array 5 :element-type 'denomination)
			#'denomination
			hand-v))

(defun suit-h (hand-v)
  (declare (type hand-v hand-v))
  (map-into (make-array 5 :element-type 'suit)
			#'suit
			hand-v))

(defun type-count-suit (hand-v)
  (declare (type hand-v hand-v))
  (let ((tc-s (make-array 4 :element-type 'tcount)))
    (loop for suit across (suit-h hand-v)
	   do (incf (aref tc-s suit)))
    tc-s))

|#
;; This feels wrong. I know much more about the structure here than I use.
#+mrtest-2-nil
(defun hand-cv (hand-v)
  #+cmu(declare (optimize (ext:inhibit-warnings 0)))
  (multiple-value-bind (cv-dr tcd-ret prim-cv-dr)
		       (t-of-tcd hand-v)
    (declare (type cv-dr-v cv-dr))
    (let ((straightp (straightp tcd-ret))
	  (flushp (flushp hand-v)))
      (setf (aref cv-dr 0)
	    ;; I get warnings here. Keep them, since this is crap.
	    (cond ((member 5 prim-cv-dr) 9)
		  ((and straightp flushp) 8) ; Fixup 5-high straight
		  ((member 4 prim-cv-dr) 7)
		  ((and (member 3 prim-cv-dr)
			(member 2 prim-cv-dr)) 6)
		  (flushp 5)
		  (straightp 4) ; Fixup 5-high straight
		  ((member 3 prim-cv-dr) 3)
		  ((= 2 (count 2 prim-cv-dr)) 2)
		  ((member 2 prim-cv-dr) 1)
		  (t 0)))
      cv-dr)))
#+mrtest-2-nil
(defun prim-cv-dr (cv-dr tcd-ret)
  (declare (type cv-dr-v cv-dr)
	   (type tcd tcd-ret)
	   (values cons))
  (loop for pos of-type array-index across (the cv-dr-v cv-dr)
    unless (member pos seen :test #'=) collect (aref tcd-ret pos) into ret
    else collect 0 into ret
    collect pos into seen
    finally (return ret)))


