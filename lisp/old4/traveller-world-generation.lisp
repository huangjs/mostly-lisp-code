(defun roll-die (sides)
  (1+ (random sides)))

(set-dispatch-macro-character
 #\# #\d
 (lambda (stream char times)
   (declare (ignore char))
   (let ((sides (read stream t nil t))
         (times (or times 1)))
     `(loop repeat ,times summing (roll-die ,sides)))))

(defmacro generate-world (&rest rules)
  `(let* ,(loop for (name . rest) in rules
	     for arrow = (member '-> rest)
	     for body = (cdr arrow)
	     for transform = (car (ldiff rest arrow))
	     collect `(,name ,@(if transform
				   `((funcall ,transform ,@body))
				   body)))
     (list ,@(mapcar #'car rules))))

(defun range (&key from to)
  (lambda (value)
    (when (numberp value)
      (min to (max from value)))))

(defun in-range (value &key from to)
  (and (numberp value)
       (<= from value to)))

(defun choose (n list)
  (nth (1- n) list))

(defun table (alist key)
  (or (cdr (assoc key alist))
      (error "Can't find key ~S in table ~S." key alist)))

(defun collecting (&rest values)
  (remove nil values))

(defparameter *starport-tech*
  '((A . 6)
    (B . 4)
    (C . 2)
    (D . 0)
    (E . 0)
    (X . -4)))

(defparameter *government-tech*
  '((0  . 1)
    (1  . 0)
    (2  . 0)
    (3  . 0)
    (4  . 0)
    (5  . 1)
    (6  . 0)
    (7  . 0)
    (8  . 0)
    (9  . 0)
    (10 . 1)
    (11 . 1)
    (12 . 1)
    (13 . -2)))

(defun my-worlds-generator ()
  (generate-world
   (starport                             -> (choose (1- #2d6) '(A A A B B C C D E E X)))
   (size (range :from 0 :to 10)          -> (- #2d6 2))
   (atmosphere (range :from 0 :to 12)    -> (if (= 0 size) 0 (+ #2d6 -7 size)))
   (hydrographics (range :from 0 :to 10) -> (if (= 0 size)
                                                0
                                                (+ #2d6 -7 atmosphere
                                                   (if (in-range atmosphere :from 3 :to 8)
                                                       -4
                                                       0))))
   (population (range :from 0 :to 10)    -> (- #2d6 2))
   (government (range :from 0 :to 13)    -> (+ #2d6 -7 population))
   (law (range :from 0 :to 9)            -> (+ #2d6 -7 government))
   (dash                                 -> '-)
   (tech-level                           -> (+ #1d6
                                               (table *starport-tech* starport)
                                               (choose (1+ size) '(2 2 1 1 1 0 0 0 0 0 0))
                                               (choose (1+ atmosphere) '(1 1 1 1 0 0 0 0 0 0 1 1 1))
                                               (choose (1+ hydrographics) '(0 0 0 0 0 0 0 0 0 1 2))
                                               (choose (1+ population) '(0 1 1 1 1 1 0 0 0 2 4))
                                               (table *government-tech* government)))
   (naval-base                           -> (choose (1- #2d6) '(~ ~ ~ ~ ~ ~ N N N N N)))
   (scout-base                           -> (choose (1- #2d6) '(~ ~ ~ ~ ~ S S S S S S)))
   (gas-giant                            -> (choose (1- #2d6) '(G G G G G G G G ~ ~ ~)))
   (trade-codes                          -> (collecting
                                             (and (in-range atmosphere :from 4 :to 9)
                                                  (in-range hydrographics :from 4 :to 8)
                                                  (in-range population :from 5 :to 7)
                                                  'Ag)
                                             (and (<= atmosphere 3)
                                                  (<= hydrographics 3)
                                                  (>= population 6)
                                                  'Na)
                                             (and (member atmosphere '(0 1 2 4 7 9))
                                                  (>= population 9)
                                                  'In)
                                             (and (<= population 6)
                                                  'Ni)
                                             (and (in-range government :from 4 :to 9)
                                                  (member atmosphere '(6 8))
                                                  (in-range population :from 6 :to 8)
                                                  'Ri)
                                             (and (in-range atmosphere :from 2 :to 5)
                                                  (<= hydrographics 3)
                                                  'Po)
                                             (and (= 10 hydrographics)
                                                  'Wa)
                                             (and (= 0 hydrographics)
                                                  'De)
                                             (and (= 0 atmosphere)
                                                  'Va)
                                             (and (= 0 size)
                                                  'As)
                                             (and (member atmosphere '(0 1))
                                                  (>= hydrographics 1)
                                                  'Ic)))))  
