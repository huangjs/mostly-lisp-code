(declaim (inline char-to-ascii-byte))

(defparameter *ascii* " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

(defun char-to-ascii-byte (char)
  (declare (type (simple-array character (*)) *ascii*))
  (let ((p (position char *ascii* :test (function char=))))
    (if p (the fixnum (+ 32 p)) (error "Not an ASCII character: ~C" char))))

(defun uri-decode (uri-string)
  "decode uri-string into octets."
  (declare (optimize (speed 3) (safety 0) (debug 0))
		   (type (simple-array character (*)) uri-string))
  (loop
     :with buffer = (make-array (length uri-string)
                                :fill-pointer 0 :element-type '(unsigned-byte 8))
     :with i = 0
     :while (< i (length uri-string))
     :do (if (char= #\% (aref uri-string i))
             (if (< (- (length uri-string) 3) i)
                 (error "% appears to close to the end of the URI: ~S"
                        uri-string)
                 (progn
                   (vector-push (parse-integer uri-string :radix 16
                                               :start (+ 1 i) :end (+ 3 i)
                                               :junk-allowed nil)
                                buffer)
                   (incf i 3)))
             (progn
               (vector-push (char-to-ascii-byte (aref uri-string i)) buffer)
               (incf i)))
     :finally (return buffer)))		  ; or (return (copy-seq buffer)) 

(defun unescape-as-uri (uri-string)
  "turn uri string into lisp string."
  #+sbcl
  (sb-ext:octets-to-string (uri-decode uri-string)))
