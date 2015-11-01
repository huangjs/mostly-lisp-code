(in-package :generator)

;;; generator.lisp version
(defgenerator firstn (g n)
  (dotimes (i n)
	(yield (funcall (generator-iterator g)))))

(defgenerator intsform (i)
  (loop
	 (yield i)
	 (incf i)))
