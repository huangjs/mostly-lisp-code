;:*=======================
;:* beginning of cpu emulator

;;; constructor and accessor
(defclass comet2 ()
  ((main-memory :initarg :main-memory :initform (make-array 65535 :element-type 'fixnum :initial-element 0 :adjustable nil) :accessor main-memory)
   (gr :initarg :gr :initform (make-array 8 :element-type 'fixnum :initial-element 0 :adjustable nil) :accessor gr)
   (sp :initarg :sp :initform 65535 :accessor sp)
   (pr :initarg :pr :initform 0 :accessor pr)
   (fr :initarg :fr :initform #*000 :accessor fr)))

(defmethod of ((cpu comet2))
  (aref (fr cpu) 0))

(defmethod (setf of) (new-val (cpu comet2))
  (setf (aref (fr cpu) 0) new-val))

(defmethod sf ((cpu comet2))
  (aref (fr cpu) 1))

(defmethod (setf sf) (new-val (cpu comet2))
  (setf (aref (fr cpu) 1) new-val))

(defmethod zf ((cpu comet2))
  (aref (fr cpu) 2))

(defmethod (setf zf) (new-val (cpu comet2))
  (setf (aref (fr cpu) 2) new-val))

(defmethod show-status ((cpu comet2))
  (format t "~&The register status of cpu comet2 is:~%")
  (format t "********************************************************~%")
  (loop for i from 0 below (length (gr cpu)) do
        (format t "GR[~d]: ~d~%" i (aref (gr cpu) i)))
  (format t "********************************************************~%")
  (format t "SP: ~d~%" (sp cpu))
  (format t "PR: ~d~%" (pr cpu))
  (format t "FR[OF]: ~d~%" (of cpu))
  (format t "FR[SF]: ~d~%" (sf cpu))
  (format t "FR[ZF]: ~d~%" (zf cpu))
  (values))

(defmethod  initialize ((cpu comet2))
  (loop for i from 0 below (length (main-memory cpu)) do
        (setf (aref (main-memory cpu) i) 0))
  (loop for i from 0 below (length (gr cpu)) do
        (setf (aref (gr cpu) i) 0))
  (setf (sp cpu) 65535)
  (setf (pr cpu) 0)
  (setf (fr cpu) #*000))

(alias reset initialize)

;;; cpu definition
(defvar *cpu* (make-instance 'comet2))

;;; synonym
(defmacro define-gr (&optional (cpu *cpu*))
  `(progn 
    ,@(loop for i from 0 below (length (gr cpu)) collect
            (let ((fn (symbol 'gr i)))
              `(defun ,fn (cpu)
                (aref (gr cpu) ,i))))))

(defmacro define-setfable-gr (&optional (cpu *cpu*))
  `(progn
    ,@(loop for i from 0 below (length (gr cpu)) collect
            (let ((fn (symbol 'gr i)))
              `(defun (setf ,fn) (new-val cpu)
                (setf (aref (gr cpu) ,i) new-val))))))

(define-gr)
(define-setfable-gr)


;;; some primitives
(defun norm-ar (x)
  "Normalize x arithmetically. If x is out of range, normalize x into -32768 ~ 32767.
The return value is (values result overflow?)."
  (cond ((> x 32767)
         (values (mod x 32768) 1))      ;overflow
        ((< x -32768)
         (values (- (mod x 32768) 32768) 1)) ;overflow
        (t (values x 0))))

(defun norm-log (x)
  "Normalize x logically. If x is out of range, normalize x into 0 ~ 65535.
The return value is (values result overflow?)."
  (cond ((or (> x 65535) (< x 0))
         (values (mod x 65536) 1))
        (t (values x 0))))

(defun c2l (x)
  "Complemental to Logical"
  (if (>= x 0)
      x
      (+ 65536 x)))

(defun l2c (x)
  "Logical to Complemental"
  (if (< x 32768)))

;;; beginning of command definiton
;:*=======================
;:* important notes
;;* the inner representation of value is LOGICAL.
;;* so that the schema of any ARITHMETIC operation is :
;;* (l2c operand)
;;* (command)
;;* (c2l operand)

;;; tricks:
;;; eg. ld r1, r2
;;; here the counterpart of r1 in Lisp will be (aref (gr *cpu*) 1) or (gr1)
;;; and we simply need to replace r1 in the definition of ld.
;;; this is actually a macro.
;;; so we need to define r0~r7 as '(aref (gr *cpu*) 0~7)
;;; there will be no problem for address, just pass in as a number

;;; define of r1 ~ r7
(defmacro define-rx (&optional (cpu *cpu*))
  `(progn 
    ,@(loop for i from 0 below (length (gr cpu)) collect
            (let ((fn (symbol 'gr i)))
              ;; note! gr1 invokes (gr cpu) not (gr *cpu*)
              ;; every command will take an optional argument _cpu_
              `(defparameter ,fn '(aref (gr cpu) ,i))))))

(define-rx)

;;; so that every command will be defined as a macro
;;; and there is not need to parse the command,
;;; simply replace every arguments with its value.

;;; memory operation
