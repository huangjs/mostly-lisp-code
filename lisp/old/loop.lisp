;;;example
(loop
    for item in list
    for i from 1 to 10
    do (something))


;;; collect
(loop for i upto 10 collect i)
(loop for i downto -10 collect i)
(loop for i from 0 downto -10 collect i)

;;; repeat
;;; for i from 1 to number-form
;;; equals
;;; repeat number-form

;;; looping over collections and packages
(loop for i in (list 10 20 30 40) collect i) ;--> (10 20 30 40)
(loop for i in (list 10 20 30 40) by #'cddr collect i)  ;--> (10 30)
(loop for x on (list 10 20 30) collect x) ;--> ((10 20 30) (20 30) (30))
(loop for x on (list 10 20 30 40) by #'cddr collect x) ;--> ((10 20 30 40) (30 40))
(loop for x accross "abcd" collect x) ;--> (#\a #\b #\c #\d)

;;; looping over hash tables
;;; (loop for <var> being the <things> in <hash-or-package> ...)


;;; equals-then iteration
;;; (loop for <var> = <initial-value-form> [then <step-form>] ...)
(loop repeat 5
      for x = 0 then y
      for y = 1 then (+ x y)
      collect y)			;--> (1 2 4 8 16)

(loop repeat 5
      for y = 1 then (+ x y)
      for x = 0 then y
      collect y)			;--> (1 1 2 4 8)

(loop repeat 5
      for x = 0 then y
      and y = 1 then (+ x y)
      collect y)			;--> (1 1 2 3 5)

;;; local variables
;;; with <var> [= <value-form>]


;;; destructing variables
(loop for (a b) in '((1 2) (3 4) (5 6))
      do (format t "a: ~a; b: ~a~%" a b))

;;; ignore values in a destructured list
(loop for (a nil) in '((1 2) (3 4) (5 6)) collect a) ;--> (1 3 5) 


;;; value accumulation
(defparameter *random* (loop repeat 100 collect (random 10000)))

(loop for i in *random*
      counting (evenp i) into evens
      counting (oddp i) into odds
      summing i into total
      maximizing i into max
      minimizing i into min
      finally (return (list min max total evens odds)))



;;; conditional execution
(loop for i from 1 to 10 do (when (evenp i) (print i)))

;;; but sum cannot be used in the previous form, we can use conditonal execution of loop
(loop for i from 1 to 10 when (evenp i) sum i)

;;; collect use conditional execution, it = result of (gethash key some-hash)
(loop for key in some-list when (gethash key some-hash) collect it)

;;; with else, unless
(loop for i from 1 to 100
      if (evenp i)
      	minimize i into min-even and
        maximize i into max-even and
        unless (zerop (mod i 4))
        	sum i into even-not-fours-total
        end
        and sum i into even-total
      else
      	minimize i into min-odd and
        maximize i into max-odd and
        when (zerop (i mod 5))
        	sum i into fives-total
        end
        and sum i into odd-total
      do (update-analysis min-even
                          max-even
                          min-odd
                          max-odd
                          even-total
                          odd-total
                          fives-total
                          even-not-fours-total))

 
