(in-package :cl-user)

(defun adler-a (bytes)
  (mod
   (1+
    (loop for d across bytes
          sum d))
   65521))

(defun adler-b (bytes)
  (mod
   (loop for d across bytes
         for i from 1
         sum (adler-a (subseq bytes 0 i)))
   65521))

(defun adler32 (bytes)
  (+ (* 65536 (adler-b bytes)) (adler-a bytes)))

