;;;=========================================================================
;;; Prints out a solution to the N-queens problem, for any N >= 4. A simple
;;; implementation of an algorithm from the SIGART bulletin, so feel free to
;;; do whatever you would like with this code. Uses the CLtL/2 loop macro.
;;;
;;; Invoke by calling (N-Queens <Board-Size>). Eg:
;;; > (N-Queens 16)
;;; -----------------------------------
;;; | _ Q _ _ _ _ _ _ _ _ _ _ _ _ _ _ |
;;; | _ _ _ Q _ _ _ _ _ _ _ _ _ _ _ _ |
;;; | _ _ _ _ _ Q _ _ _ _ _ _ _ _ _ _ |
;;; | _ _ _ _ _ _ _ Q _ _ _ _ _ _ _ _ |
;;; | _ _ _ _ _ _ _ _ _ Q _ _ _ _ _ _ |
;;; | _ _ _ _ _ _ _ _ _ _ _ Q _ _ _ _ |
;;; | _ _ _ _ _ _ _ _ _ _ _ _ _ Q _ _ |
;;; | _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ Q |
;;; | Q _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ |
;;; | _ _ Q _ _ _ _ _ _ _ _ _ _ _ _ _ |
;;; | _ _ _ _ Q _ _ _ _ _ _ _ _ _ _ _ |
;;; | _ _ _ _ _ _ Q _ _ _ _ _ _ _ _ _ |
;;; | _ _ _ _ _ _ _ _ Q _ _ _ _ _ _ _ |
;;; | _ _ _ _ _ _ _ _ _ _ Q _ _ _ _ _ |
;;; | _ _ _ _ _ _ _ _ _ _ _ _ Q _ _ _ |
;;; | _ _ _ _ _ _ _ _ _ _ _ _ _ _ Q _ |
;;; -----------------------------------
;;;
;;; 1991 Marty Hall marty_hall@jhuapl.edu.
;;;=========================================================================

;;;=========================================================================
;;; From algorithm in SIGART Bulletin, Vol 2, Number 2, page 7. Determines 
;;; where to place each queen in constant time, so it takes longer to print
;;; out the board than to do the calculations. I am not aware of a 
;;; non-exponential approach that finds ALL solutions, as this just finds 
;;; ONE solution. Do not specify a value for ``Extra-Space?'' when
;;; calling this from the top-level.
;;; 4/91 Marty Hall

(defun N-Queens (N &optional Extra-Space?)
  (cond
    ((and (evenp N) (not (integerp (/ (- N 2) 6))))
     (Even-Queens-1 N Extra-Space?))
    ((evenp N)
     (Even-Queens-2 N Extra-Space?))
    (t
      (Odd-Queens N))
     ))

;;;========================================================================
;;; N even but not of form 6K+2.

(defun Even-Queens-1 (N &optional Extra-Space?)
  (let ((M (if Extra-Space? (1+ N) N)))
    (Print-Border M)
    (loop for I from 1 to (/ N 2) do
      (Print-Row M (* 2 I)))
    (loop for I from 1 to (/ N 2) do
      (Print-Row M (1- (* 2 I))))
    (unless Extra-Space? (Print-Border N))
    (values)
))

;;;========================================================================
;;; N even but not of form 6K

(defun Even-Queens-2 (N &optional Extra-Space?)
  (let ((M (if Extra-Space? (1+ N) N)))
    (Print-Border M)
    (loop for I from 1 to (/ N 2) do
      (Print-Row M (1+ (Queen-Mod I N))))
    (loop for I from (/ N 2) downto 1 do
      (Print-Row M (- N (Queen-Mod I N))))
    (unless Extra-Space? (Print-Border N))
    (values)
))

;;;=========================================================================

(defun Queen-Mod (I N)
  (mod (+ (* 2 (1- I)) (/ N 2) -1) N))

;;;=========================================================================
;;; For odd N, just do N-1 case and then place queen on (N,N).

(defun Odd-Queens (N)
  (N-Queens (1- N) t)
  (Print-Row N N)
  (Print-Border N)
  (values)
)

;;;=========================================================================

(defun Print-Border (N)
  (fresh-line)
  (dotimes (I (+ N 1))
    (princ "--"))
  (princ "-")
)

;;;=========================================================================
;;; Prints a "Q" at Position, blanks otherwise.

(defun Print-Row (Length Position)
  (fresh-line)
  (princ "|")
  (dotimes (I (1- Position))
    (princ " _"))
  (princ " Q")
  (dotimes (I (- Length Position))
    (princ " _"))
  (princ " |")
)

;;;======================================================================

(format t "~%Finds A (not ALL) solutions to any given size of the N-Queens~%~
             problem. Finds the position of each queen in constant time, so~%~
             the limiting step is printing out the board which is obviously~%~
             O(N^2).  Invoke with  (N-Queens <N>). ")

;;;======================================================================
