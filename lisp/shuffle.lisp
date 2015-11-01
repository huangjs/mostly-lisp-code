;; http://okmij.org/ftp/Haskell/perfect-shuffle.txt

;; imperative approach

(defun shuffle-vector (v)
  (let ((size (length v)))
    (loop for i below size
          for candidate = (+ i (random (- size i)))
          do (rotatef (aref v i) (aref v candidate)))))

