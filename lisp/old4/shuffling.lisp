(defun seqrnd (seq)
  "Randomize the elements of a sequence. Destructive on SEQ."
  (sort seq #'> :key (lambda (x) (random 1.0))))
 
;; (seqrnd "abcd")
;; -> "dacb"

