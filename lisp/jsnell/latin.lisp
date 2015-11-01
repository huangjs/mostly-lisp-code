(defun permutations (list)
  (cond ((null list)
         nil)
        ((null (cdr list))
         (list list))
        (t
         (loop with head = (car list)
               for l in (permutations (cdr list))
               for list = (copy-list (cons head l))
               append (loop for i on list
                            collect (copy-list list)
                            until (null (cdr i))
                            do (rotatef (car i) (cadr i)))))))

(defun create-permutation-strings-and-masks (n list)
  (loop for p in (permutations list)
        collect (cons
                 (make-array n
                             :element-type '(unsigned-byte 8)
                             :initial-contents (mapcar (lambda (x)
                                                         (+ (char-code #\1) x))
                                                       p))
                 (loop with a = (make-array (* n n)
                                            :element-type 'bit
                                            :initial-element 0)
                       for i fixnum from 0 by n
                       for v fixnum in p
                       do (setf (bit a (+ i v)) 1)
                       finally (return a)))))

(defvar *count* 0)

(defun latin (n output)
  (declare (type (mod 16) n))
  (let* ((list (loop for i below n collect i))
         (*print-pretty* nil)
         (output-permutations (permutations list))
         (output-count (length output-permutations))
         (permutations (create-permutation-strings-and-masks n list))
         (set (make-array n))
         (mask (make-array (* n n) :element-type 'bit)))
    (labels ((output-permutations ()
               (dolist (output-permutation output-permutations)
                 (let ((first t))
                   (dolist (output-index output-permutation)
                     (if first
                         (setf first nil)
                         (write-byte (char-code #\:) output))
                     (write-sequence (aref set output-index) output)))
                 (write-byte (char-code #\Newline) output)))
             (aux (depth permutations)
               (declare (type (mod 16) depth)
                        (optimize speed))
               (if (> depth n)
                   (if output
                       (output-permutations)
                       (incf *count* output-count))
                   (loop for ((p-print . p-mask) . rest) on permutations
                         do (locally
                                (declare (type simple-bit-vector p-mask))
                              (unwind-protect
                                   (progn
                                     (bit-xor mask p-mask mask)
                                     (when (= (count 1 mask)
                                              (* depth n))
                                       (setf (aref set (1- depth)) p-print)
                                       (aux (1+ depth) rest)))
                                (bit-xor mask p-mask mask)))))))
      (aux 1 permutations))))

(defun test (n output)
  (if output
      (with-open-file (output "/dev/stdout"
                              :direction :output
                              :if-exists :append
                              :if-does-not-exist :error
                              :element-type '(unsigned-byte 8))
        (latin n output))
      (progn
        (latin n output)
        (format t "found ~a ~ax~a latin squares~%" *count* n n))))
