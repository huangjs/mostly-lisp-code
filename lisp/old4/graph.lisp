;;; -*- indent-tabs-mode: nil -*-

;;; Copyright (c) 2007 David Lichteblau
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(defpackage :heapgraph
  (:use :cl))

(in-package :heapgraph)

(defconstant +n+ sb-vm:n-word-bytes)
(defconstant +2n+ (* 2 +n+))

(sb-alien:define-alien-variable "sizetab" (array (* t) 256))

(defstruct
    (ctx (:constructor make-ctx (stream customizer
                                        &key (worklist (cons nil nil))
                                             (worklist-tail worklist))))
  stream
  (fixups '())
  (force (make-hash-table))
  customizer
  (addresses (make-hash-table))
  (weak-pointers '())
  (worklist (error "oops"))
  (worklist-tail (error "oops")))

(defmethod print-object ((object ctx) stream)
  (print-unreadable-object (object stream)))

(defvar *show-fixups* nil)

(defun dump-object
    (object pathname
     &key size show-fixups (if-exists :error) customizer (force t))
  (with-open-file (s pathname
                     :direction :output
                     :if-does-not-exist :create
                     :if-exists if-exists)
    (write-line "digraph G {" s)
    (when size
      (format s "size=\"~F, ~F\";~%" size size))
    (let ((ctx (make-ctx s customizer))
          (*show-fixups* show-fixups))
      (dolist (arg (if (eq force t) (list object) force))
        (setf (gethash arg (ctx-force ctx)) t))
      (dump-all object ctx)
      (update-weak-pointers ctx))
    (write-line "}" s)))

(defun dump-all (object ctx)
  (prog1
      (sub-dump-object object ctx)
    (loop while (cdr (ctx-worklist ctx)) do
         (pop (ctx-worklist ctx))
         (destructuring-bind (p &rest refs)
             (car (ctx-worklist ctx))
           (dolist (ref refs)
             (write-edge ctx p (sub-dump-object ref ctx)))))))

(defun update-weak-pointers (ctx)
  (dolist (wp (ctx-weak-pointers ctx))
    (multiple-value-bind (value alive)
        (sb-ext:weak-pointer-value wp)
      (let* ((value-address
              (when alive
                (gethash value (ctx-addresses ctx)))))
        (cond
          (value-address
            ;; value has been dumped, write its address
           )
          (t
            ;; break it
           ))))))

(defun native-address (object)
  (logandc2 (sb-kernel:get-lisp-obj-address object) sb-vm:lowtag-mask))

(defun native-pointer (object)
  (sb-sys:int-sap (native-address object)))

(defun object-ref-word (object index)
  (sb-sys:without-gcing
   (sb-sys:sap-ref-word (native-pointer object) (* index +n+))))

(defun (setf object-ref-word) (newval object index)
  (sb-sys:without-gcing
   (setf (sb-sys:sap-ref-word (native-pointer object) (* index +n+))
         newval)))

(defun object-ref-lispobj (object index)
  (sb-sys:without-gcing
   (sb-kernel:make-lisp-obj
    (sb-sys:sap-ref-word (native-pointer object) (* index +n+)))))

(defun align (address)
  (- address (nth-value 1 (ceiling address (1+ sb-vm:lowtag-mask)))))

(defun make-address (raw-pointer lowtag)
  (logior raw-pointer lowtag))

(defun forcep (object ctx)
  (or (gethash object (ctx-force ctx))
      (etypecase object
        (package nil)
        (symbol
          (or (null (symbol-package object))
              (forcep (symbol-package object) ctx)))
        (sb-kernel:classoid (forcep (sb-kernel:classoid-name object) ctx))
        (sb-kernel:layout (forcep (sb-kernel:layout-classoid object) ctx))
        (sb-kernel:fdefn
          (let ((name (sb-kernel:fdefn-name object)))
            (or (not (fixupable-function-p
                      (sb-kernel:fdefn-fun object)
                      name
                      ctx))
                ;; fixme: isn't this vaguely like !fixupable-function-p (but
                ;; worse, not exactly the same)?  Should it be?
                (typecase name
                  (symbol (and (symbolp name) (forcep name ctx)))
                  (list
                    (or (some (lambda (x) (and (symbolp x) (forcep x ctx)))
                              name)
                        ;; always dump ctor fdefns
                        (eq 'sb-pcl::ctor (car name))
                        ;; ditto for accessors
                        (eq 'sb-pcl::slot-accessor (car name))))
                  (t nil)))))
        (sb-kernel:named-type
          (let ((name (sb-kernel:named-type-name object)))
            (and (symbolp name) (forcep name ctx))))
        (sb-kernel:array-type
          nil)
        (class
          (or (not (slot-boundp object 'sb-pcl::name)) ;argh.  FIXME!
              (forcep (class-name object) ctx)))
        (function nil))))

(defun slot-accessor-p (gf)
  (let ((x (sb-mop:generic-function-name gf)))
    (and (listp x) (eq (car x) 'sb-pcl::slot-accessor))))

(defun dump-fixup (object ctx)
  (multiple-value-bind (kind description)
      (etypecase object
        (package
         (values :package (package-name object)))
        (symbol
         (values :symbol (write-to-string object)))
        (sb-kernel:classoid
         (values :classoid (sb-kernel:classoid-name object)))
        (sb-kernel:layout
         (values :layout (sb-kernel:classoid-name
                          (sb-kernel:layout-classoid object))))
        (sb-kernel:fdefn
         (values :fdefn (sb-kernel:fdefn-name object)))
        (sb-kernel:named-type
         (values :named-type
                 (sb-kernel:named-type-name object)))
        (sb-kernel:array-type
         (values :array-type
                 (list :dimensions
                       (sb-kernel::array-type-dimensions object)
                       :complexp
                       (sb-kernel::array-type-complexp object)
                       :element-type
                       (sb-kernel::array-type-element-type object)
                       :specialized-element-type
                       (sb-kernel::array-type-specialized-element-type
                        object))))
        (class (values :class (class-name object)))
        (generic-function
         (if (slot-accessor-p object)
             (values :slot-accessor
                     (sb-mop:generic-function-name object))
             (values :generic-function
                     (sb-mop:generic-function-name object))))
        (sb-pcl::ctor
         (values :ctor
                 (list* (sb-pcl::ctor-function-name object)
                        (sb-pcl::ctor-class-name object)
                        (sb-pcl::ctor-initargs object))))
        (function
         (assert (eql (sb-kernel:widetag-of object)
                      sb-vm:simple-fun-header-widetag))
         (values :function
                 (sb-kernel:%simple-fun-name object))))
    (%write-fixup ctx object kind description)))

(defun write-node (ctx id size label extra)
  (format (ctx-stream ctx) "~A [label=~S ~A];~%"
          id
          (format nil "~A ~A" size label)
          extra))

(defun write-edge (ctx id1 id2)
  (unless (or (eq id2 :immediate) (and (get id2 :fixup) (not *show-fixups*)))
    (format (ctx-stream ctx) "~A -> ~A" id1 id2)
    (when (get id2 :fixup)
      (write-string " [style=dotted]" (ctx-stream ctx)))
    (write-line ";" (ctx-stream ctx))))

(defun %write-fixup (ctx object kind description)
  (let ((id (gensym)))
    (setf (get id :fixup) t)
    (when *show-fixups*
      (write-node ctx
                  id
                  :fixup
                  (format nil "~A ~A" kind description)
                  ", color=grey"))
    (setf (gethash object (ctx-addresses ctx)) id)
    id))

(defun function-name-identifier (name)
  (cond
    ((symbolp name)
      name)
    ((and (listp name)
          (eq (car name) 'setf)
          (symbolp (second name)))
      (second name))))

(defun fixupable-function-p (fn name ctx)
  (let ((id (function-name-identifier name)))
    (and (not (forcep fn ctx))          ;fixme: check other entry-points, too?
         id
         (not (forcep id ctx))
         (not (and (listp name) (eq (car name) 'sb-pcl::fast-method)))
         (let ((fdefn (sb-int:info :function :definition name)))
           (and fdefn (eq fn (sb-kernel:fdefn-fun fdefn)))))))

(defun simplify-type (type)
  (cond
    ((and (listp type)
          (eq (car type) 'simple-array)
          (subtypep (second type) 'integer))
      '(simple-array "subtype of integer"))
    ((subtypep type 'string)
     'string)
    ((and (subtypep type 'simple-array) (listp type))
      (list (car type) "foo"))
    (t
      type)))

(defun sub-dump-object (object ctx &key fixup-only)
  (cond
    ;; already seen
    ((gethash object (ctx-addresses ctx)))
    ;; immediate
    ((or (null object)
         (eq object t)
         (evenp (sb-kernel:lowtag-of object)))
     :immediate)
    ;; customizer/user-defined fixups
    ((and (ctx-customizer ctx)
          (multiple-value-bind (dumpp data1 data2)
              (funcall (ctx-customizer ctx) object)
            (ecase dumpp
              ((t) nil)
              ((nil)
                (setf (gethash object (ctx-addresses ctx))
                      (sub-dump-object data1 ctx :fixup-only fixup-only)))
              (:fixup
               (%write-fixup ctx :user (list data1 data2)))))))
    ;; other fixup, unless overriden
    ((and (typep object '(or package symbol class sb-kernel:layout
                          sb-kernel:classoid sb-kernel:fdefn
                          sb-kernel:named-type sb-kernel:array-type))
          (not (forcep object ctx)))
      (dump-fixup object ctx))
    ;; functions
    ((and (functionp object)
          (eql (sb-kernel:widetag-of object) sb-vm:simple-fun-header-widetag))
      ;; Funktionsobjekte muessten wir eigentlich dumpen, weil sie nicht
      ;; in dem Sinne eindeutig sind.  Wenn wir aber eine Funktion finden,
      ;; die tatsaechlich so exakt wieder ueber ihren Namen auffindbar ist,
      ;; dumpen wir mal opportunistisch doch ein Fixup um Platz zu sparen.
      ;; In vielen Faellen sollte das so ohnehin richtiger sein.
      (cond
        ((fixupable-function-p object
                               (sb-kernel:%simple-fun-name object)
                               ctx)
          (dump-fixup object ctx))
        (t
          (when fixup-only
            (return-from sub-dump-object nil))
          (sub-dump-object (simple-fun-code-object object) ctx)
          (gethash object (ctx-addresses ctx)))))
    ((and (typep object 'generic-function)
          (slot-boundp object 'sb-pcl::name)
          (or (slot-accessor-p object)  ;never dump slot accessors
              (fixupable-function-p object
                                    (sb-mop:generic-function-name object)
                                    ctx)))
      (dump-fixup object ctx))
    ((typep object 'sb-pcl::ctor)
      ;; never dump ctors
      (dump-fixup object ctx))
    ((eq object sb-impl::*physical-host*)
     (%write-fixup ctx object :variable 'sb-impl::*physical-host*))
    ;; ordinary dumpable objects
    (t
      (when fixup-only
        (return-from sub-dump-object nil))
      (let ((id (gensym)))
        (setf (gethash object (ctx-addresses ctx)) id)
        (multiple-value-bind (length refs)
            (dump-nonfixup object ctx)
          (write-node ctx id
                      length
                      (if (stringp object)
                          (write-to-string
                           (if (<= (length object) 5)
                               object
                               (concatenate 'string
                                            (subseq object 0 5)
                                            "...")))
                          (simplify-type (type-of object)))
                      (let* ((red (min 255 (* length 4)))
                             (green (truncate 0.7 red)))
                        (format nil ", color=\"#~2,'0X~2,'0X~2,'0X\", shape=box"
                                red green 0)))
          (when refs
            (push (cons id refs) (cdr (ctx-worklist-tail ctx)))
            (setf (ctx-worklist-tail ctx)
                  (cdr (ctx-worklist-tail ctx)))))
        id))))

(defun dump-nonfixup (object ctx)
  (typecase object
    (cons (dump-cons object ctx))
    ((or integer single-float double-float (complex single-float)
         (complex double-float) #+long-float (complex long-float)
         sb-sys:system-area-pointer)
      (dump-unboxed object ctx))
    ((or symbol ratio complex)
      (dump-boxed object ctx))
    (sb-kernel:funcallable-instance
      (dump-funcallable-instance object ctx))
    (simple-vector (dump-simple-vector object ctx))
    ((simple-array * (*)) (dump-primitive-vector object ctx))
    (array (dump-boxed object ctx))
    (sb-kernel:instance (dump-instance object ctx))
    (sb-kernel:code-component (dump-code-component object ctx))
    (function (dump-closure object ctx))
    (sb-kernel:fdefn (dump-fdefn object ctx))
    (sb-ext:weak-pointer
      (multiple-value-bind (value alive)
          (sb-ext:weak-pointer-value object)
        (prog1
            (dump-unboxed object ctx)
          (when alive
            (sub-dump-object value ctx
                             ;; don't dump the actual value here, but
                             ;; if it's fixupable, dump the fixup to avoid
                             ;; breaking the reference needlessly
                             :fixup-only t)
            (push object (ctx-weak-pointers ctx))))))
    (t
      (if (sb-di::indirect-value-cell-p object)
          (dump-boxed object ctx)
          (error "cannot dump object ~S" object)))))

(defun dump-cons (object ctx)
  (values +2n+ (list (car object) (cdr object))))

(defun dump-boxed (object ctx)
  (let ((len (sb-kernel:get-header-data object)))
    (values (* (1+ len) +n+)
            (loop
               for i from 1 to len
               collect (object-ref-lispobj object i)))))

(defun dump-funcallable-instance (object ctx)
  (let ((len (sb-kernel:get-closure-length object)))
    (values (* (1+ len) +n+)
            (loop
               for i from 1 to len
               collect (object-ref-lispobj object i)))))

(defun dump-unboxed (object ctx)
  (let ((len (sb-kernel:get-header-data object)))
    (values (* (1+ len) +n+) nil)))

(defun dump-simple-vector (object ctx &optional fixup)
  (let ((length (length object)))
    (values (* (+ 2 length) +n+) (coerce object 'list))))

(defun size-of (object)
  (sb-sys:with-pinned-objects (object)
    (sb-alien:with-alien
        ((fn (* (function sb-alien:long (* t)))
             (sb-sys:sap-ref-sap (sb-alien:alien-sap sizetab)
                                 (* +n+ (sb-kernel:widetag-of object)))))
      (sb-alien:alien-funcall fn (native-pointer object)))))

(defun dump-primitive-vector (object ctx)
  (values (align (* +n+ (size-of object)))))

(defun dump-instance (instance ctx)
  (when (typep instance 'hash-table)
    (assert (not (sb-impl::hash-table-weakness instance))))
  (let* ((len (sb-kernel:%instance-length instance))
         (layout (sb-kernel:%instance-layout instance))
         (nuntagged (sb-kernel:layout-n-untagged-slots layout)))
    (values (* (1+ len) +n+)
            (loop
               for i from 0 below (- len nuntagged)
               collect
               (sb-kernel:%instance-ref instance i)))))

(defun simple-fun-code-object (fun)
  (sb-sys:with-pinned-objects (fun)
    (let* ((fun-sap (native-pointer fun))
           (header-value
            (ash (sb-sys:sap-ref-word fun-sap 0) (- sb-vm:n-widetag-bits))))
      (sb-kernel:make-lisp-obj
       (logior (- (sb-sys:sap-int fun-sap) (* header-value +n+))
               sb-vm:other-pointer-lowtag)))))

;; fixme: can this be done by DUMP-PACKAGE?
(defun note-fast-method-plist (fun ctx) fun ctx
  #+#.(cl:if (cl:find-symbol "METHOD-FUNCTION-PLIST" :sb-pcl) '(and) '(or))
  (let ((plist (sb-pcl::method-function-plist fun)))
    (when plist
      (%write-fixup ctx :fast-method (cons fun plist)))))

(defun dump-code-component (code ctx)
  (let* ((simple-funs
          (loop
             for fun = (sb-kernel:%code-entry-points code)
             :then (sb-kernel:%simple-fun-next fun)
             while fun
             collect fun))
         (n-header-words (sb-kernel:get-header-data code))
         (n-code-words (sb-kernel:%code-code-size code))
         (n-bytes (align (* +n+ (+ n-header-words n-code-words)))))
    ;; we register the simple-funs here since they don't dump themselves
    (sb-sys:with-pinned-objects (code)
      (let* ((old-address (native-address code)))
        (dolist (fun simple-funs)
          (setf (gethash fun (ctx-addresses ctx))
                (gethash code (ctx-addresses ctx))))))
    (values n-bytes
            (sb-sys:with-pinned-objects (code)
              (let* ((old-address (native-address code))
                     (code-sap (sb-sys:int-sap old-address))

                     #+x86
                     (old-end-address (+ old-address n-bytes))
                     (data (make-array n-bytes :element-type '(unsigned-byte 8))))
                #+sb-heapdump
                (dolist (ref (gethash code *foreign-fixups*))
                  (%write-fixup ctx :foreign ref))
                ;; fixme: can this be done by DUMP-PACKAGE?
                (dolist (fun simple-funs)
                  (let ((name (sb-kernel:%simple-fun-name fun)))
                    (when (and (listp name) (eq (car name) 'sb-pcl::fast-method))
                      (note-fast-method-plist fun ctx))))
                (append (loop
                           for i from 1 below n-header-words
                           collect (object-ref-lispobj code i))
                        (loop for fun in simple-funs
                           append
                           (let ((x (truncate (- (native-address fun) old-address) +n+)))
                             (loop
                                for i from (+ x 2) below (+ x sb-vm:simple-fun-code-offset)
                                collect (object-ref-lispobj code i))))))))))

(defun dump-closure (object ctx)
  (let ((len (sb-kernel:get-closure-length object)))
    (note-fast-method-plist object ctx)
    (values (* (1+ len) +n+)
            (cons (sb-kernel:%closure-fun object)
                  (loop
                     for i from 2 to len
                     collect (object-ref-lispobj object i))))))

(defun dump-fdefn (object ctx)
  (let ((len (sb-kernel:get-header-data object)))
    (values (* (1+ len) +n+)
            (list (sb-kernel:fdefn-name object)
                  (sb-kernel:fdefn-fun object)))))
