;;; Consistent hash values for pointer-based identity
;;;
;;; On a non-generational GC, this would be a weak EQ hash table.
;;;
;;; With a generational GC, it's silly to rehash everything at every GC.
;;;
;;; Instead, maintain a list of records per generation, and only process
;;; generations that have been GCed and potentially moved/dropped, and epoch
;;; markers for each generation (and a global epoch marker).
;;;
;;; The invariant is that the data for each generation is correct if the generational
;;; epoch marker is the same as that in the generational info.
;;;
;;; To add a record, we have something like
;;; 0. epoch marker has already been stashed
;;; 1. compute the record
;;; 2. enqueue the record
;;;
;;; This is safe because: the object is pinned, so the only information that matters
;;; is in which generation it is. If a GC has happened before 1, we have the right
;;; info. If it's happened after 1, and the generation has been GCed, the epoch marker
;;; for that generation has changed.
;;;
;;; Finally, this is way too slow, so we use a hash table to ensure quick per-address
;;; lookups.
;;;
;;; The global epoch marker protects that hash table. When a GC has happened since,
;;; the data in the hash table might be stale.
;;;
;;; In that case, partial rebuild:
;;; Set new GC epoch
;;; For each generation, check the epoch, if still OK, do nothing, else push records
;;; in to-rehash queue and update epoch.
;;;
;;; Go through the to-rehash queue and update the hash.
;;;
;;; Issue: Could be susceptible to live lock from really frequent GCs.
;;; Solution: pin the object around all operations, that way any GC won't change the
;;;  object's address, and the hash table is correct, if only for that object.
;;;
;;; Locking policy: lock around hash and maybe-insert.
;;; Monotonous if only insert/read.
;;; For rehash, trick is to remove mappings before updating the epoch markers.
;;; If before update to epoch markers, readers will try to rehash and re-read.
;;; If after, readers will find an empty value and try to insert.
;;;
(defpackage "CONSISTENT-EQ2"
    (:use "CL" "SB-VM" "SB-KERNEL" "SB-SYS" "SB-EXT" "SB-ALIEN")
  (:export "MAKE-EQ-MAP" "GET-ID" "EQ-MAP"))
(in-package "CONSISTENT-EQ2")

#+goog
(progn
  (declaim (inline fixnum-sap fixnum-alien sap-fixnum alien-fixnum))
  (defun fixnum-sap (x)
    (declare (type fixnum x))
    (int-sap (get-lisp-obj-address x)))
  (defun fixnum-alien (x)
    (sap-alien (fixnum-sap x) (* t)))
  (defun sap-fixnum (x)
    (declare (type system-area-pointer x))
    (the fixnum (%make-lisp-obj (sap-int x))))
  (defun alien-fixnum (x)
    (sap-fixnum (alien-sap x)))

  (sb-alien:load-shared-object "/Users/pkhuong/cdensehash.dylib")

  (declaim (inline %map-build %map-destroy %map-clear %map-get %map-erase %map-set))
  (define-alien-routine ("map_build" %map-build) (* t) (size unsigned-long) (mul unsigned-long))
  (define-alien-routine ("map_destroy" %map-destroy) void (map (* t)))
  (define-alien-routine ("map_clear" %map-clear) void (map (* t)))
  (define-alien-routine ("map_get" %map-get) unsigned-long
    (map (* t)) (key unsigned-long) (default unsigned-long))
  (define-alien-routine ("map_erase" %map-erase) unsigned-long (map (* t)) (key unsigned-long))
  (define-alien-routine ("map_set" %map-set) void (map (* t)) (key unsigned-long) (value unsigned-long))

  (defun map-build (&optional (size 16) mul)
    (alien-fixnum (%map-build size (or mul
                                       (random (ash 1 64))))))
  (defun map-destroy (map)
    (%map-destroy (fixnum-alien map)))
  (declaim (inline map-clear map-get map-erase map-set))
  (defun map-clear (map)
    (%map-clear (fixnum-alien map)))
  (defun map-get (map key default)
    (%make-lisp-obj (%map-get (fixnum-alien map)
                              (get-lisp-obj-address key) (get-lisp-obj-address default))))
  (defun map-erase (map key)
    (%map-erase (fixnum-alien map) (get-lisp-obj-address key)))
  (defun map-set (map key value)
    (%map-set (fixnum-alien map) (get-lisp-obj-address key) (get-lisp-obj-address value))
    value))

(deftype index ()
  `(mod ,most-positive-fixnum))
(deftype positive-fixnum ()
  '(and unsigned-byte fixnum))
(deftype table ()
  #- (or goog fx) 'hash-table
  #+goog 'fixnum
  #+fx    'hash:hash)

(defstruct (eq-map
             (:constructor %make-eq-map)
             (:include sb-thread:mutex))
  
  (table     #- (or goog fx) (make-hash-table :test #'eql) #+goog (error "Foo") #+fx (hash:make 16)
   :type table)
  (epoch     sb-kernel::*gc-epoch*          :type list)
  (gen-epoch  (copy-seq sb-kernel::**gc-epoch-per-generation**)
   :type (simple-array list (#. (1+ +highest-normal-generation+))))
  (gen-head   (make-array (1+ +highest-normal-generation+) :initial-element -1 :element-type 'fixnum)
   :type (simple-array fixnum (#. (1+ +highest-normal-generation+))))
  (entries-data (make-array  16) :type simple-vector) ; weak pointer, value
  (entries-next (make-array  16  :element-type 'fixnum) :type (simple-array fixnum 1)) ; address, next
  (entries-count 0     :type index) ; *2
  (random-state     (make-random-state)            :type random-state)
  (version-counter 0   :type positive-fixnum)
  (last-full-rehash-version 0 :type positive-fixnum))

(defun make-eq-map (&optional size)
  (setf size (or size 16))
  (let* ((table #- (or goog fx) (make-hash-table :test 'eql :size (or size 16))
                #+goog (map-build (or size 16))
                #+fx (hash:make size))
         (map   (%make-eq-map :table table
                              :entries-data (make-array (* 2 size))
                              :entries-next (make-array (* 2 size) :element-type 'fixnum))))
    #+goog
    (finalize map (lambda ()
                    (format t "destroy: ~A~%" table)
                    (map-destroy table)))
    map))

(declaim (inline %fixnum-address %object-dynamic-heap-p %object-generation))
(defun %fixnum-address (x)
  (declare (optimize speed))
  (truly-the fixnum
             (%make-lisp-obj (logandc2 (get-lisp-obj-address x)
                                       fixnum-tag-mask))))
(defun %object-dynamic-heap-p (x)
  (declare (optimize speed))
  (let ((addr (get-lisp-obj-address x)))
    (and (oddp addr)
         (< (sb-kernel:current-dynamic-space-start) addr
            (sb-sys:sap-int (sb-kernel:dynamic-space-free-pointer))))))

(defun %object-generation (x)
  (declare (optimize speed (safety 0)))
  (let ((addr (get-lisp-obj-address x)))
    (let ((index (sb-vm::find-page-index addr)))
      (symbol-macrolet ((page (sb-alien:deref sb-vm::page-table index)))
        (sb-alien:slot page 'sb-vm::gen)))))

(declaim (inline %%push-eq-mapping %push-eq-mapping)) ; only used once...
(defun %%push-eq-mapping (weak-object value address map gen)
  (declare (type eq-map map)
           (type (integer 0 #.sb-vm:+highest-normal-generation+) gen)
           (optimize speed (sb-c::insert-array-bounds-checks 0)))
  (let* ((head    (eq-map-gen-head map))
         (data    (eq-map-entries-data map))
         (next    (eq-map-entries-next map))
         (count   (eq-map-entries-count map))
         (len     (length next)))
    (declare (type simple-vector data)
             (type (simple-array fixnum 1) next)
             (type index count))
    (when (>= count len)
      (setf data (replace (make-array (* 2 len)) data)
            (eq-map-entries-data map) data
            next (replace (make-array (* 2 len) :element-type 'fixnum) next)
            (eq-map-entries-next map) next))
    (setf (aref data       count) weak-object
          (aref data  (1+ count)) value
          (aref next  (1+ count)) address)
    (shiftf (aref next count)   (aref head gen) count)
    (incf (eq-map-entries-count map) 2)
    map))

(defun %push-eq-mapping (object value map gen)
  (declare (type eq-map map)
           (type (integer 0 #.sb-vm:+highest-normal-generation+) gen)
           (optimize speed))
  (%%push-eq-mapping (make-weak-pointer object) value (%fixnum-address object)
                     map gen))

(declaim (inline %get-id %set-id))
(defun %get-id (object eq-map)
  (declare (type eq-map eq-map)
           (optimize speed))
  #- (or goog fx)
  (gethash (%fixnum-address object) (eq-map-table eq-map))
  #+goog
  (map-get (eq-map-table eq-map) (%fixnum-address object) nil)
  #+fx
  (hash:get (eq-map-table eq-map) (%fixnum-address object)))

(defun %set-id (object eq-map)
  (declare (type eq-map eq-map)
           (optimize speed))
  (let* ((address (%fixnum-address object))
         (value   (random (1+ most-positive-fixnum) (eq-map-random-state eq-map))))
    (let* ((gen (%object-generation object)))
      (%push-eq-mapping object value eq-map gen)
      #- (or goog fx)
      (setf (gethash address (eq-map-table eq-map)) value)
      #+goog
      (map-set (eq-map-table eq-map) address value)
      #+fx
      (hash:set (eq-map-table eq-map) address value))
    value))

(declaim (inline map-epoch-ok))
(defun map-epoch-ok (eq-map)
  (declare (type eq-map eq-map)
           (optimize speed))
  (eql sb-kernel::*gc-epoch* (eq-map-epoch eq-map)))

(defun slow-get-id (object map)
  (declare (type eq-map map))
  (sb-thread:with-mutex (map)
    (unless (map-epoch-ok map) ; GC has happened, rehash, and then we 
      (rehash-eq-map map))     ; can lookup/insert: object is pinned
    (or (%get-id object map)
        (prog2 (setf (eq-map-version-counter map)
                     (logand (1+ (eq-map-version-counter map)) most-positive-fixnum)) ;; membar!
            (%set-id object map)
          (setf (eq-map-version-counter map)
                (logand (1+ (eq-map-version-counter map)) most-positive-fixnum))))))

(declaim (maybe-inline get-id))
(defun get-id (object eq-map)
  (declare (type eq-map eq-map))
  (with-pinned-objects (object)
    (let ((address (get-lisp-obj-address object))
          (version0 (eq-map-version-counter eq-map)))
      (flet ((slow-path ()
               (if (< (sb-kernel:current-dynamic-space-start) address
                      (sb-sys:sap-int (sb-kernel:dynamic-space-free-pointer)))
                   (slow-get-id object eq-map)
                   (sb-impl::eql-hash object))))
        (cond ((evenp address)
               (sb-impl::eql-hash object))
              ((oddp version0)
               (slow-path))
              (t
               (let ((value (%get-id object eq-map)))
                 (if (and value (map-epoch-ok eq-map) ; no GC has happened since the
                          (= version0 (eq-map-version-counter eq-map)))  ;  first operation on the map
                     value
                     (slow-path)))))))))

;; Rehashing logic!
;;
;; Idea:
;;  If we're rehashing, it's because the GC epoch has changed. Every reader will try to rehash as 
;;  well. Readers and writers are locked out.
;;
;; So, we can just do whatever we want until we reset the map's GC epoch marker, since everyone
;; is locked out.
;;
;; So: read the new epoch marker, and *then* the generation-specific epoch markers
;;  do our stuff
;;  set epoch marker
;;    * generation-specific markers don't matter, as long they're set to a value from
;;      before the entries are processed              
;;  return

(defun partial-clear (table map)
  (declare (type table table)
           (type eq-map map)
           (optimize speed (sb-c::insert-array-bounds-checks 0)))
  (let ((entries     -1)
        (epoch       sb-kernel::**gc-epoch-per-generation**)
        (markers     (eq-map-gen-epoch map))
        (heads       (eq-map-gen-head  map))
        (data        (eq-map-entries-data map))
        (next        (eq-map-entries-next map)))
    (declare (type fixnum entries))
    (dotimes (i (1+ +highest-normal-generation+))
      (when (eql (aref markers i) (aref epoch i))
        (go NEXT))
      (setf (aref markers i) (aref epoch i))
      (when (minusp (aref heads i))
        (go NEXT))
      (let* ((head (shiftf (aref heads i) -1))
             (tail (shiftf entries head)))
        (loop while (>= head 0)
              do
           (let* ((next-entry (aref next head))
                  (address    (aref data (+ head 1))))
             #- (or goog fx)
             (remhash address table)
             #+goog
             (map-erase table address)
             #+fx
             (hash:clear table address)
             (when (minusp next-entry)
               (setf (aref next head) tail))
             (setf head next-entry))))
      NEXT)
    entries))

(defun partial-rebuild (table map head)
  (declare (type table table)
           (type eq-map map)
           (type fixnum head))
  (let ((heads       (eq-map-gen-head  map))
        (data        (eq-map-entries-data map))
        (next        (eq-map-entries-next map)))
    (loop while (>= head 0)
          do
       (let* ((next-entry   (aref next head))
              (weak-pointer (aref data head)))
         (multiple-value-bind (key livep) (weak-pointer-value weak-pointer)
           (when livep
             (with-pinned-objects (key)
               (let ((address (%fixnum-address key))
                     (gen     (%object-generation key))
                     (val     (aref data (1+ head))))
                 #- (or goog fx)
                 (setf (gethash address table) val)
                 #+goog
                 (map-set table address val)
                 #+fx
                 (hash:set table address val)
                 (setf (aref next (1+ head)) address)
                 (shiftf (aref next head) (aref heads gen) head))))
           (setf head next-entry))
         (setf head next-entry)))))

(defun full-rebuild (table map data count)
  (declare (type table table)
           (type eq-map map)
           (type simple-vector data)
           (type index count))
  (loop for i below count by 2
        do
     (let ((wp (aref data i)))
       (multiple-value-bind (key livep) (weak-pointer-value wp)
         (when livep
           (with-pinned-objects (key)
             (let ((address (%fixnum-address key))
                   (gen     (%object-generation key))
                   (val     (aref data (1+ i))))
               #- (or goog fx)
               (setf (gethash address table) val)
               #+goog
               (map-set table address val)
               #+fx
               (hash:set table address val)
               (%%push-eq-mapping wp (aref data (1+ i)) address map gen)))))))
  map)

(defun full-clear (table map)
  (declare (type table table)
           (type eq-map map))
  (let ((data  (eq-map-entries-data map))
        (count (eq-map-entries-count map)))
    (setf (eq-map-entries-data map) (make-array (length data))
          (eq-map-gen-epoch map)    (copy-seq sb-kernel::**gc-epoch-per-generation**)
          (eq-map-entries-count map) 0
          (eq-map-last-full-rehash-version map) (eq-map-version-counter map))
    #- (or goog fx)
    (clrhash table)
    #+goog
    (map-clear table)
    #+fx
    (hash:reset table)
    (values data count)))

(defun rehash-eq-map (map)
  (declare (type eq-map map)
           (optimize speed (sb-c::insert-array-bounds-checks 0)))
  (let* ((begin-epoch sb-kernel::*gc-epoch*)
         (table       (eq-map-table map)))
    (cond ((and (eql (aref (eq-map-gen-epoch map) +highest-normal-generation+)
                     (aref sb-kernel::**gc-epoch-per-generation** +highest-normal-generation+))
                (<= (logand (- (eq-map-version-counter map) (eq-map-last-full-rehash-version map))
                            most-positive-fixnum)
                    (eq-map-entries-count map)))
           (partial-rebuild table map (partial-clear table map)))
          (t
           (multiple-value-bind (data count) (full-clear table map)
             (full-rebuild table map data count))))
    (setf (eq-map-epoch map) begin-epoch)
    map))

#||
Examples
CONSISTENT-EQ2> (defparameter *conses* (map-into (make-array (* 16 1024 1024))
                                                (lambda ()
                                                  (list nil))))
*CONSES*
CONSISTENT-EQ2> (defun test-hash (gc-p)
                 (let ((hash (prog1 (make-hash-table :test #'eql :weakness :key :synchronized t)
                               (gc :full t)))
                       (cons (list nil)))
                   (time (progn
                           (setf (gethash cons hash) 0)
                           (map nil (lambda (cons)
                                      (setf (gethash cons hash) 0))
                                *conses*)))
                   (when gc-p
                     (gc :gen 4)
                     (setf (sb-impl::hash-table-needs-rehash-p hash) t))
                   (sb-vm::with-cycle-counter (let (x)
                                                (loop repeat 1 do (setf x (gethash cons hash)))
                                                x))))
TEST-HASH
CONSISTENT-EQ2> (defun test-map (gc-p)
                 (let ((map (prog1 (make-eq-map) (gc :full t)))
                       (cons (list nil)))
                   (time (progn (get-id cons map)
                                (map nil (lambda (cons)
                                           (get-id cons map))
                                     *conses*)))
                   (when gc-p
                     (gc :gen 4)
                     (setf sb-kernel::*gc-epoch* (list nil)
                           (aref sb-kernel::**gc-epoch-per-generation** 1) (list nil)))
                   (sb-vm::with-cycle-counter (let (x)
                                                (loop repeat 1 do (setf x (get-id cons map)))
                                                x))))
TEST-MAP
CONSISTENT-EQ2> (test-hash t)
Evaluation took:
  9.139 seconds of real time
  9.088568 seconds of total run time (7.532471 user, 1.556097 system)
  [ Run times consist of 4.364 seconds GC time, and 4.725 seconds non-GC time. ]
  99.45% CPU
  25,523,121,446 processor cycles
  2,013,270,656 bytes consed
  
0
1134528880
CONSISTENT-EQ2> (test-map t)
Evaluation took:
  24.979 seconds of real time
  24.933559 seconds of total run time (21.889368 user, 3.044191 system)
  [ Run times consist of 12.741 seconds GC time, and 12.193 seconds non-GC time. ]
  99.82% CPU
  69,766,525,080 processor cycles
  1,073,801,280 bytes consed
  
939312464744081903
76744
CONSISTENT-EQ2> (/ 1134528880 76744d0)
14783.290941311372d0

;; with fx hash tables
CONSISTENT-EQ2> (test-map t)
Evaluation took:
  18.132 seconds of real time
  18.101131 seconds of total run time (15.056941 user, 3.044190 system)
  [ Run times consist of 8.633 seconds GC time, and 9.469 seconds non-GC time. ]
  99.83% CPU
  50,642,868,628 processor cycles
  6,032,847,568 bytes consed
  
146405826465135485
22036
||#
