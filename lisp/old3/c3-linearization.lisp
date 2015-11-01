;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-

;;; Copyright © 2003 Paul Foley (mycroft@actrix.gen.nz)
;;; 
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this Software to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; provided that the above copyright notice and this permission notice
;;; are included in all copies or substantial portions of the Software.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
;;; USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;; DAMAGE.

#|
;;; example

(defclass boat () ())
(defclass day-boat (boat) ())
(defclass engine-less (day-boat) ())
(defclass pedal-wheel-boat (engine-less) ())
(defclass wheel-boat (boat) ())
(defclass pedal-wheel-boat (engine-less wheel-boat) ())
(defclass small-multinull (day-boat) ())
(defclass small-catamaran (small-multinull) ())
(defclass pedalo (pedal-wheel-boat small-catamaran) ())
(sb-pcl:compute-class-precedence-list (find-class 'pedal-wheel-boat))
(sb-pcl:compute-class-precedence-list (find-class 'pedalo))

|#

#|
;;; examples				; ;

(defclass boat () ())
(defclass day-boat (boat) ())
(defclass engine-less (day-boat) ())
(defclass pedal-wheel-boat (engine-less) ())
(defclass wheel-boat (boat) ())
(defclass pedal-wheel-boat (engine-less wheel-boat) ())
(defclass small-multinull (day-boat) ())
(defclass small-catamaran (small-multinull) ())
(defclass pedalo (pedal-wheel-boat small-catamaran) ())

(sb-pcl:compute-class-precedence-list (find-class 'pedal-wheel-boat))
(sb-pcl:compute-class-precedence-list (find-class 'pedalo))

(defclass boat () () (:metaclass c3-class))
(defclass day-boat (boat) () (:metaclass c3-class))
(defclass engine-less (day-boat) () (:metaclass c3-class))
(defclass pedal-wheel-boat (engine-less) () (:metaclass c3-class))
(defclass wheel-boat (boat) () (:metaclass c3-class))
(defclass pedal-wheel-boat (engine-less wheel-boat) () (:metaclass c3-class))
(defclass small-multinull (day-boat) () (:metaclass c3-class))
(defclass small-catamaran (small-multinull) () (:metaclass c3-class))
(defclass pedalo (pedal-wheel-boat small-catamaran) () (:metaclass c3-class))

|#

#+CMU (import 'kernel::topological-sort)

#-CMU
(defun topological-sort (elements constraints tie-breaker)
  (let ((result '()))
    (loop
       (let* ((rhs (mapcar #'cdr constraints))
	      (elts (remove-if (lambda (x) (member x rhs)) elements)))
	 (when (null elts)
	   (if elements
	       (error "Inconsistent constraints in ~S" 'topological-sort)
	       (unless elements (return (nreverse result)))))
	 (let ((elt
		(if (cdr elts) (funcall tie-breaker elts result) (car elts))))
	   (push elt result)
	   (setq elements (delete elt elements))
	   (setq constraints (delete-if (lambda (x)
					  (or (eq (car x) elt)
					      (eq (cdr x) elt)))
					constraints)))))))

(defun std-cpl-tie-breaker (free-classes rev-cpl)
  (dolist (class rev-cpl (first free-classes))
    (let* ((superclasses (sb-mop:class-direct-superclasses class))
	   (intersection (intersection free-classes superclasses)))
      (when intersection
	(return (first intersection))))))

#+(and CMU18 (not CMU18E)) (shadowing-import 'pcl::standard-class "PCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass dylan-class (standard-class) ())

(defun compute-dylan-cpl (class tie-breaker)
  (let* ((supers (sb-mop:class-direct-superclasses class))
	 (classes (list* class supers))
	 (constraints (mapcar #'cons (list* class supers) supers)))
    (dolist (cpl (mapcar #'sb-mop:class-precedence-list supers))
      (setf classes (append cpl classes))
      (setf constraints (nconc (mapcar #'cons cpl (cdr cpl)) constraints)))
    (setf classes (delete-duplicates classes))
    (setf constraints (delete-duplicates constraints :test #'equal))
    (topological-sort classes constraints tie-breaker)))

(defmethod sb-mop:compute-class-precedence-list ((class dylan-class))
  (compute-dylan-cpl class #'std-cpl-tie-breaker))

(defmethod sb-mop:validate-superclass ((class dylan-class)
				       (new-super standard-class))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass c3-class (standard-class) ())

(defun c3-tie-breaker (free-classes rev-cpl)
  (dolist (super (sb-mop:class-direct-superclasses (car (last rev-cpl))))
    (dolist (item free-classes)
      (when (member item (sb-mop:class-precedence-list super))
	(return-from c3-tie-breaker item)))))

(defmethod sb-mop:compute-class-precedence-list ((class c3-class))
  (compute-dylan-cpl class #'c3-tie-breaker))

(defmethod sb-mop:validate-superclass ((class c3-class)
				       (new-super standard-class))
  t)
