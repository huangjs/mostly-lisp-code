(defvar *core* nil)
(defvar *spaces* nil)
(defvar *initial-function* nil)

(defstruct sbcl-space
  name
  words
  file-location
  memory-start
  memory-size
  memory)

(defun read-core (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (let ((core (make-array (list (file-length stream))
                            :element-type '(unsigned-byte 8))))
      (read-sequence core stream)
      core)))

(defun core-word (index)
  (loop with res = 0
        for i from (1- sb-vm::n-word-bytes) downto 0 do
        (setf res (logior (ash res 8)
                          (aref *core* (+ index i))))
        finally (return res)))

(defun assert-core-char (index char)
  (assert (= (aref *core* index) (char-code char))))

(defun assert-core-word (index word)
  (assert (= (core-word index) word)))

(defun initialize-spaces (file)
  (let ((*core* (read-core file))
        (index 0))
    (labels ((next-word (&optional (amount 1))
               (incf index (* amount sb-vm::n-word-bytes)))
             (assert-next-word (word)
               (assert-core-word index word)
               (next-word))
             (read-word ()
               (prog1
                   (core-word index)
                 (next-word)))
             (read-space (name id)
               (let ((space (make-sbcl-space :name name)))
                 (assert-next-word id)
                 (setf (sbcl-space-words space) (read-word))
                 (setf (sbcl-space-file-location space)
                       (* sb-c:*backend-page-size* (1+ (read-word))))
                 (setf (sbcl-space-memory-start space)
                       (* sb-c:*backend-page-size* (read-word)))
                 (setf (sbcl-space-memory-size space)
                       (* sb-c:*backend-page-size* (read-word)))
                 (setf (sbcl-space-memory space)
                       (subseq *core*
                               (sbcl-space-file-location space)
                               (+ (sbcl-space-file-location space)
                                  (sbcl-space-memory-size space))))
                 (assert (= (sbcl-space-memory-size space)
                            (length (sbcl-space-memory space))))
                 (setf (getf *spaces* name) space)
                 space)))
      ;; core magic
      (assert-core-char 0 #\L)
      (assert-core-char 1 #\C)
      (assert-core-char 2 #\B)
      (assert-core-char 3 #\S)
      (next-word)
      ;; version type code
      (assert-next-word 3860)
      ;; version
      (assert-next-word 3)
      (assert-next-word 3)
      ;; build id type code
      (assert-next-word 3899)
      (next-word (1- (core-word index)))
      ;; new-directory-core-entry-type-code
      (assert-next-word 3861)
      (assert-next-word 17)
      (read-space :read-only 3)
      (read-space :static 2)
      (read-space :dynamic 1)
      ;;initial-fun-core-entry-type-code
      (assert-next-word 3863)
      (assert-next-word 3)
      (setf *initial-function* (read-word))
      ;; end-core-entry-type-code 
      (assert-next-word 3840)
      (values))))

(defun lowtag-of (addr)
  (logand addr sb-vm::lowtag-mask))

(defun widetag-of (addr)
  (let ((a (logandc2 addr sb-vm::lowtag-mask)))
    (logand (iref a) sb-vm::widetag-mask)))

(defun header-value (addr)
  (let ((a (logandc2 addr sb-vm::lowtag-mask)))
    (ash (iref a) -8)))

(defun slot-ref (addr n)
  (iref (+ addr (* sb-vm::n-word-bytes n))))

(defun primitive-object-p (addr)
  (not (eq (widetag-of addr) sb-vm::instance-header-widetag)))

(defun primitive-object-pointer-p (addr)
  (not (eq (image-type-of addr) 'instance-pointer)))

(defun primitive-object-by-widetag (widetag)
  (cond ((= widetag sb-vm::code-header-widetag)
         (car (remove-if-not
               (lambda (x) (eq (sb-vm::primitive-object-name x)
                               'sb-vm::code))
               sb-vm::*primitive-objects*)))
        (t
         (loop for prim in sb-vm::*primitive-objects* do
               (when (eq (symbol-value (sb-vm::primitive-object-widetag prim))
                         widetag)
                 (return-from primitive-object-by-widetag prim))))))
        
(defun object-slots (addr)
  (cond ((primitive-object-p addr)
         (let* ((widetag (widetag-of addr))
                (prim (primitive-object-by-widetag widetag)))
           (loop for slot in (sb-vm::primitive-object-slots prim)
                 for name = (slot-value slot 'sb-vm::name)
                 for offset = (slot-value slot 'sb-vm::offset)
                 for rest-p = (slot-value slot 'sb-vm::rest-p)
                 for slot-index from 1
                 collect name
                 collect (if rest-p
                             (loop for i from offset
                                   repeat (- (header-value addr)
                                             slot-index)
                                   collect (slot-ref addr i))
                             (slot-ref addr offset)))))
        (t
         (error "foo"))))

(defun object-pointer-slots (value)
  (object-slots (logandc2 value sb-vm::lowtag-mask)))
  
(defun image-type-of (addr)
  (let ((lowtag (lowtag-of addr)))
    (ecase lowtag
      ((#.sb-vm::even-fixnum-lowtag #.sb-vm::odd-fixnum-lowtag)
       'fixnum)
      ((#.sb-vm:other-immediate-0-lowtag
        #.sb-vm:other-immediate-1-lowtag
        #.sb-vm:other-immediate-2-lowtag
        #.sb-vm:other-immediate-3-lowtag)
       'other-immediate)
      (#.sb-vm:list-pointer-lowtag
       'cons)
      (#.sb-vm:fun-pointer-lowtag
       'function-pointer)
      (#.sb-vm:other-pointer-lowtag
       'other-pointer)
      (#.sb-vm:instance-pointer-lowtag
       'instance-pointer))))

(defun object-size (addr)
  (labels ((iref-next (&optional (bits sb-vm::n-word-bits))
             (ceil (+ (truncate (iref (+ addr sb-vm::n-word-bytes))
                                (truncate sb-vm::n-word-bits bits))
                      2)))
           (ceil (value)
             (if (oddp value)
                 (1+ value)
                 value)))
    (let ((header (iref addr)))
      (ecase (image-type-of header)
        ((fixnum function-pointer other-pointer instance-pointer cons) 1)
        (other-immediate
         (let ((widetag (logand header sb-vm::widetag-mask)))
           (ecase widetag
             ((#.sb-vm::code-header-widetag)
              (ceil (+ (header-value addr)
                       (ash (iref-next) (- sb-vm:n-fixnum-tag-bits)))))
             ((#.sb-vm::value-cell-header-widetag
               #.sb-vm::symbol-header-widetag
               #.sb-vm::fdefn-widetag
               #.sb-vm::instance-header-widetag
               #.sb-vm::funcallable-instance-header-widetag
               #.sb-vm::complex-base-string-widetag
               #.sb-vm::complex-character-string-widetag
               #.sb-vm::complex-bit-vector-widetag
               #.sb-vm::complex-array-widetag
               #.sb-vm::simple-array-widetag
               #.sb-vm::ratio-widetag
               #.sb-vm::bignum-widetag
               #.sb-vm::closure-header-widetag)
              (ceil (header-value addr)))
             ((#.sb-vm::character-widetag
               #+x86-64 #.sb-vm::single-float-widetag
               #.sb-vm::unbound-marker-widetag)
              sb-vm::n-word-bytes)
             (#.sb-vm::complex-widetag
              (ceil (1+ (header-value addr))))
             (#.sb-vm::weak-pointer-widetag
              sb-vm::weak-pointer-size)
             (#.sb-vm::complex-vector-widetag
              (iref-next))
             (#.sb-vm::simple-vector-widetag
              (iref-next))
             #+sb-unicode
             (#.sb-vm::simple-character-string-widetag
              (iref-next 32))
             (#.sb-vm::simple-base-string-widetag
              (iref-next 8)))))))))

(defun iref (addr)
  (loop for space in (cdr *spaces*) by #'cddr
        for index = (- addr (sbcl-space-memory-start space))
        do
        (when (and (or (zerop index)
                       (plusp index))
                   (< index 
                      (length (sbcl-space-memory space))))
          (loop with res = 0
                for i from (1- sb-vm::n-word-bytes) downto 0 do
                (setf res (logior (ash res 8)
                                  (aref (sbcl-space-memory space)
                                        (+ index i))))
                finally (return-from iref res))))
  (error "oops?"))

(defun iref-byte (addr)
  (loop for space in (cdr *spaces*) by #'cddr
        for index = (- addr (sbcl-space-memory-start space))
        do
        (when (and (or (zerop index)
                       (plusp index))
                   (< index 
                      (length (sbcl-space-memory space))))
          (print index)
          (return-from iref-byte (aref (sbcl-space-memory space)
                                       index))))
  (error "oops?"))

(defun analyze-image (file &optional initialize)
  (when (or initialize
            (not *spaces*))
    (initialize-spaces file))
  (image-type-of *initial-function*))

(defun map-space-objects (space fun)
  (loop with addr = (sbcl-space-memory-start space)
        with end =  (+ (sbcl-space-memory-start space)
                       (sbcl-space-memory-size space))
        until (= addr end)
        do
        (tagbody
         retry
           (restart-case
            (progn
              (assert (< addr end))
              (let ((size (* sb-vm::n-word-bytes (object-size addr))))
                (when (> size 10000)
                  (format t "~x / ~x~%" size addr))
                (assert (not (zerop size)))
                (incf addr size)))
            (retry ()
                   (format t "addr=~x~%" addr)
                   (go retry))))))
          
  
(defun map-image-objects (fun)
  (loop for space in (cdr *spaces*) by #'cddr
        append (map-space-objects space fun)))
  


