;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: (SYSDEF); -*-

(defpackage "SYSDEF"
  (:use "CL")
  (:import-from #.(package-name 
           (or (find-package "CLOS")
               (find-package "PCL")
               (error "Can't find suitable CLOS package.")))
   ;; In CLISP, this is an internal symbol.  In other implementations,
   ;; this is an external symbol.  :import-from ignores that distinction.
   "CLASS-PRECEDENCE-LIST")
  (:export
   "CREATE-SYSTEM"
   "DEFINE-SYSTEM"
   "EXECUTE-ACTION"
   "EXECUTE-PLAN"
   "EXPLAIN-ACTION"
   "EXPLAIN-PLAN"
   "FIND-SYSTEM-NAMED"
   "FULL-NAME"
   "GENERATE-PLAN"
   "MODULAR-SYSTEM"
   "MODULE"
   "MODULE-ASSERTIONS"
   "MODULE-CAUSES"     
   "MODULE-NEEDS"
   "MODULE-SPEC"
   "MODULE-SYSTEM"
   "MODULES"
   "NAME"
   "PREPLANNED-SYSTEM"
   "PROCESS-OPTION"
   "PROCESS-OPTIONS"
   "SHORT-NAME"
   "SIMPLE-SYSTEM"
   "SOURCE-FILES"
   "SYSTEM"
   "SYSTEM-PLANS"
   "VANILLA-SYSTEM"
   ))

(in-package "SYSDEF")


;;;; General protocol

(defgeneric name (entity))


;;;; System

;;; SYSTEM
;;;
;;; Any class that claims to satisfy the SYSTEM protocol should
;;; use this class as a superclass.

(defclass system () ())

;; The system protocol.

(defgeneric short-name (system))
(defgeneric full-name  (system))

(defgeneric process-options (system options))
(defgeneric process-option (system option-key &rest option-data))
(defgeneric source-files (system))
(defgeneric generate-plan (system plan-key &rest plan-data))
(defgeneric explain-plan (system plan))
(defgeneric execute-plan (system plan))
(defgeneric explain-action (system action-key &rest action-data))
(defgeneric execute-action (system action-key &rest action-data))

;; If CL had a way of declaring these to be required methods, 
;; we'd use that.  Instead, we at least make these give better 
;; error messages as default methods for the protocol class SYSTEM.
;; Note that we don't do this on class T, since some generics
;; have names like NAME that are likely to be used in other packages
;; than ours in ways we can't anticipate, so we keep our definitions
;; local to the classes we create.

(defmethod name ((system system))
  (error "The required method NAME is not implemented by ~S."
     system))

(defmethod short-name ((system system))
  (error "The required method SHORT-NAME is not implemented by ~S."
     system))

(defmethod full-name ((system system))
  (error "The required method FULL-NAME is not implemented by ~S."
     system))

(defmethod process-options ((system system) options)
  (error "The required method PROCESS-OPTIONS is not implemented by ~S."
     system))

(defmethod process-option ((system system) option-name &rest option-args)
  (error "The required method PROCESS-OPTION is not implemented by ~S."
     system))

(defmethod source-files ((system system))
  (error "The required method SOURCE-FILES is not implemented by ~S."
     system))

(defmethod generate-plan ((system system) plan-key &rest plan-args)
  (error "The required method GENERATE-PLAN is not implemented by ~S."
     system))

(defmethod explain-plan ((system system) plan)
  (error "The required method EXPLAIN-PLAN is not implemented by ~S."
     system))

(defmethod execute-plan ((system system) plan)
  (error "The required method EXECUTE-PLAN is not implemented by ~S."
     system))

(defmethod explain-action ((system system) action-name &rest action-args)
  (error "The required method EXPLAIN-ACTION is not implemented by ~S."
     system))

(defmethod execute-action ((system system) action-name &rest action-args)
  (error "The required method EXECUTE-ACTION is not implemented by ~S."
     system))


;;;; Vanilla System

;;; VANILLA-SYSTEM
;;;
;;; A vanilla system knows about names and how to process options,
;;; but has no interesting options it is willing to process that would
;;; make it useful as something to instantiate.

(defclass vanilla-system (system)
  ((short-name :initarg :short-name 
           :accessor short-name
           :initform nil)
   (full-name  :initarg :full-name
           :accessor full-name
           :initform nil)))

;;; (NAME vanilla-system)
;;;
;;; Returns the name of the system
;;; Long name is preferred over short name where both are available.

(defmethod name ((system vanilla-system))
  (or (full-name system) (short-name system)))

;;; (PRINT-OBJECT vanilla-system stream)
;;;
;;; For debugging convenience,
;;; (PRIN1 mysys) types something like: #<SYSTEM "My System" 343324>
;;; (PRINC mysys) types something like: My System

(defmethod print-object ((system vanilla-system) stream)
  (let ((my-name (name system)))
    (if *print-escape*
    (print-unreadable-object (system stream :type t :identity t)
          (format stream "~S" my-name))
    (format stream "~A" my-name))))

;;; (DESCRIBE-OBJECT vanilla-system stream)
;;;
;;; Does that part of the explanation relevant to the class.
;;; Other classes mixing this in should use :AFTER or :BEFORE methods.

(defmethod describe-object ((system vanilla-system) stream)
  (with-slots (full-name short-name) system
    (format stream "~2&~S:~%The system named ~A~@[, or ~A,~] is of class ~S.~%" 
        system 
            full-name 
            (unless (equal full-name short-name) short-name)
            (class-of system))
    system))


;;;; Options Facility

;;; (PROCESS-OPTIONS vanilla-system options)
;;;
;;; Maps across the given options, digesting them.

(defmethod process-options ((system vanilla-system) options)
  (dolist (data options)
    (apply #'process-option system (car data) (cdr data)))
  system)

;;; (PROCESS-OPTION vanilla-system opt-name .  opt-args)
;;;
;;; :NAME Sets defaults for all name types.
;;; :SHORT-NAME Sets the short name (overrides :NAME if given).
;;; :LONG-NAME Sets the long name (overrides :NAME if given).
;;; otherwise Signals an error.

(defmethod process-option ((system vanilla-system) (key (eql :name)) &rest data)
  (with-slots (short-name full-name) system
    (destructuring-bind (datum) data
      (if (not short-name) (setq short-name (string datum)))
      (if (not full-name)  (setq full-name  (string datum))))))

(defmethod process-option ((system vanilla-system) (key (eql :short-name)) &rest data)
  (with-slots (short-name full-name) system
    (destructuring-bind (datum) data
      (setq short-name (string datum)))))

(defmethod process-option ((system vanilla-system) (key (eql :full-name)) &rest data)
  (with-slots (short-name full-name) system
    (destructuring-bind (datum) data
      (setq full-name (string datum)))))

(defmethod process-option ((system vanilla-system) (key t) &rest data)
  (error "The option ~S is not known to ~S.~%Data: ~S"
         key system data))


;;;; Planning/Executing Actions

;;; (GENERATE-PLAN vanilla-system action . data)
;;;
;;; This method returns abstract information about how to perform
;;; a specified ACTION. The reply is in the form of a list of the
;;; form ((ACTION-KEY .  ACTION-ARGS1) (ACTION-KEY .  ACTION-ARGS2) ...),
;;; such that executing EXECUTE-ACTION (or EXPLAIN-ACTION) of each 
;;; ACTION-KEY (with the given ACTION-ARGS) to the object in order will
;;; accomplish the action in question.
;;;
;;;  :UPDATE       How to compile (or otherwise update) the system,
;;;  :INSTANTIATE  How to load (or otherwise instantiate) the system.
;;;
;;;  Otherwise     An error results if the action isn't defined.
;;;                (Subclasses of this class might provide additional
;;;                action-keys, provided they also manage EXECUTE-ACTION
;;;                and EXPLAIN-ACTION accordingly.)

(defmethod generate-plan ((system vanilla-system) (key t) &rest data)
  (error "The object ~S does not know how to ~S.~
     ~%~@[Perhaps you want to use ~A instead?~
      ~%~]Data: ~S"
         system
         key
         (case key
           ((:load)    :instantiate)
           ((:compile) :update))
         data))

;;; (EXECUTE-PLAN vanilla-system plan)
;;;
;;; The steps of the plan are executed.
;;;
;;; PLAN may be either a plan name (a symbol) or a list of steps
;;; such as that returned by GENERATE-PLAN.

(defmethod execute-plan ((system vanilla-system) plan)
  (cond ((symbolp plan)
         (execute-plan system (generate-plan system plan)))
        (t
         (dolist (step plan)
           (apply #'execute-action system step))))
  system)

;;; (EXPLAIN-PLAN vanilla-system plan)
;;;
;;; The steps of the PLAN are explained.
;;;
;;; PLAN may be either a plan name (a symbol) or a list of steps
;;; such as that returned by GENERATE-PLAN.

(defmethod explain-plan ((system vanilla-system) plan)
  (cond ((symbolp plan)
         (explain-plan system (generate-plan system plan)))
        (t
         (dolist (step plan)
           (apply #'explain-action system step))))
  system)

;;; (EXECUTE-ACTION vanilla-system key .  data)
;;; (EXPLAIN-ACTION vanilla-system key .  data)
;;;
;;; EXECUTE-ACTION causes a given action to occur.
;;;
;;; EXPLAIN-ACTION describes what a given action would do if performed.
;;;
;;; The action key should be one of:
;;;  :LOAD      Load a (Lisp) file.
;;;  :COMPILE   Compile a (Lisp) file.
;;;  Otherwise  it's an error.

(defmethod execute-action ((system vanilla-system) (key (eql :load)) &rest data)
  (destructuring-bind (file) data
    (load file)))

(defmethod execute-action ((system vanilla-system) (key (eql :compile)) &rest data)
  (destructuring-bind (file) data
    (compile-file file)))

(defmethod execute-action ((system vanilla-system) (key t) &rest data)
  (error "The action ~S is not known to ~S.~%Data: ~S"
         key system data))

;;; (EXPLAIN-ACTION vanilla-system key . data)
;;;
;;; If the action is valid but a description wasn't available, try
;;; to conjure up a plausible description based on the name of the
;;; action and its arguments.

(defmethod explain-action ((system vanilla-system) (key t) &rest data)
  (cond ((not 
          (let ((spec2 `(eql ,key)))
            (dolist (class (class-precedence-list (class-of system)))
              ;; FIND-METHOD works on the exact class with no inheritance,
              ;; so we must simulate inheritance dynamically.
              (if (find-method #'execute-action '() (list class spec2) nil)
                  (return t)))))
         (error "The action ~S is not known to ~S, so can't describe it.~%Data: ~S"
                key system data))
        (t (format t "~&~A~@[ ~{~A~^, ~}.~]~%"
                   (string-capitalize key) data))))


;;;; Simple System

;;; SIMPLE-SYSTEM
;;;
;;; A simple system is a system which has left-to-right file
;;; dependencies.

(defclass simple-system (vanilla-system)
  ((source-files :initarg :source-files
                 :reader source-files
                 :initform '())))

;;; (DESCRIBE-OBJECT simple-system) :AFTER
;;;
;;; Tacks on some information about the files which make up this system.

(defmethod describe-object :after ((system simple-system) stream)
  (format stream "~&It has source files~{~<~%~1:; ~A~>~^,~}.~%" 
          (source-files system)))

;;; (PROCESS-OPTIONS simple-system options)
;;;
;;; The only options allowed to a simple system is a list of file
;;; names with left-to-right ordering dependencies.

(defmethod process-options ((system simple-system) options)
  (with-slots (source-files) system
    (when options
      (setq source-files '())
      (let ((default-pathname (merge-pathnames (car options))))
    (dolist (file options)
      (setq default-pathname (merge-pathnames file default-pathname))
      (push default-pathname source-files))
    (setq source-files (nreverse source-files)))
      t)))

;;; (GENERATE-PLAN simple-system :UPDATE)
;;;
;;; To update this kind of system, one must compile and load each
;;; of its files in sequence.

(defmethod generate-plan ((system simple-system) (key (eql :update)) &rest data)
  (mapcan #'(lambda (file)
              (list (list ':compile file)
                    (list ':load (compile-file-pathname file))))
          (source-files system)))

;;; (GENERATE-PLAN simple-system :INSTANTIATE)
;;;
;;; To instantiate this kind of system, one must simply load each
;;; of its files in sequence.

(defmethod generate-plan ((system simple-system) (key (eql :instantiate)) &rest data)
  (mapcan #'(lambda (file)
              (list (list ':load (make-pathname :type nil :defaults file))))
          (source-files system)))


;;;; Pre-Planned System

;;; PREPLANNED-SYSTEM
;;;
;;; A preplanned system is a system which has its plans for manipulation
;;; specified explicitly rather than inferred.

(defclass preplanned-system (vanilla-system)
  ((source-files :initarg :files :accessor source-files :initform '() )
   (system-plans :initarg :plans                        :initform '() )))

;;; (PROCESS-OPTIONS preplanned-system options)
;;;
;;; The clauses in the DEFINE-SYSTEM for this kind of system are
;;; just (<plan-name> .  <commands>).

(defmethod process-options ((system preplanned-system) options)
  (with-slots (source-files system-plans) system
    (setq source-files (car options))
    (setq system-plans (cdr options))))

;;; (GENERATE-PLAN preplanned-system operation)
;;;
;;; This does simple table-lookup to find the plan.

(defmethod generate-plan ((system preplanned-system) operation &rest data)
  (check-type data null)
  (with-slots (system-plans) system
    (let ((entry (assoc operation system-plans)))
      (cond (entry (cdr entry))
            (t (error "No plan for operation ~S" operation))))))



;;;; Modular System

;;; MODULAR-SYSTEM
;;;
;;; A modular system is a system which allows specification of
;;; inter-module dependencies, both implicit and explicit.

(defclass modular-system (vanilla-system)
  ((modules :initarg :modules
            :accessor modules
            :initform '())))

;;; (DESCRIBE-SYSTEM modular-system stream) :AFTER
;;;
;;; When a modular system is described, we tack on information
;;; saying how many modules it has and then we ask each module
;;; to describe itself.

(defmethod describe-system :after ((system modular-system) stream)
  (let* ((m-list (modules system)) (n (length m-list)))
    (format stream "~&It has ~D module~:P~C~%"
        n (if (= n 0) #\. #\:))
    (dolist (m m-list)
      (describe m))))

;;; (GET-MODULE modular-system name)
;;;
;;; Returns the component module with the given name (or NIL if none).

(defmethod get-module ((system modular-system) name)
  (dolist (m (modules system))
    (if (eq (name m) name) (return m))))

;;; (SOURCE-FILES modular-system)
;;;
;;; Returns a list of the source files for the system.

(defmethod source-files ((system modular-system))
  (apply #'append (mapcar #'source-files (modules system))))

;;; (PROCESS-OPTION modular-system module .  spec)
;;;
;;; Declares how to handle the :MODULE option.  Creates an object
;;; of type MODULE and lets it process the associated spec.

(defmethod process-option ((system modular-system) (key (eql :module)) &rest spec)
  (with-slots (modules) system
    (setq modules
          (nconc modules
                 (list (make-instance 'module
                      :system system
                      :spec spec))))))



;;;; Module

;;; MODULE
;;;
;;; A module is a collection of files to be used as a building
;;; block for modular systems.

(defclass module ()
  ((name         :initarg :name         :accessor name              :initform nil)
   (system       :initarg :system       :accessor module-system     :initform nil)
   (spec         :initarg :spec         :accessor module-spec       :initform nil)
   (source-files :initarg :source-files :accessor source-files      :initform '())
   (assertions   :initarg :assertions   :accessor module-assertions :initform '())
   (needs        :initarg :needs        :accessor module-needs      :initform '())
   (causes       :initarg :causes       :accessor module-causes     :initform '())))

;;; (INITIALIZE-INSTANCE module &key) :AFTER
;;;
;;; See to it that if SPEC was given, it gets appropriately processed.

(defmethod initialize-instance :after ((module module) &key)
  (process-spec module (module-spec module)))

;;; (PRINT-OBJECT module stream)
;;;
;;; For debugging convenience.
;;; (PRIN1 mod) types something like: #<Module MYSYS*MOD1 234567>
;;; (PRINC mod) types something like: MOD1

(defmethod print-object ((module module) stream)
  (with-slots (name system) module
    (if *print-escape*
    (print-unreadable-object (module stream :type t :identity t)
      (format stream "~@[~A*~]~:[Anonymous~;~:*~A~]"
          (if system (short-name system))
          name))
      (format stream "~A" name))))

;;; (DESCRIBE-OBJECT module stream)
;;;
;;; Details the source files and dependency information
;;; for the module.

(defmethod describe-object ((module module) stream)
  (with-slots (needs source-files system) module
    (format stream "~2& ~A~@[  ~{~%~A~^.~}~]~%" module source-files)
    (do ((n needs (cddr n)))
        ((null n))
      (format t "~& ~S dependenc~@P: ~{~S~^, ~}.~%"
              (car n) (length (cadr n)) (cadr n)))
    (format t "~&")
    system))

;;; (PROCESS-SPEC module spec)
;;;
;;; Process the given SPEC absorbing relevant info.
;;;
;;; The NAME is only absorbed if name info isn't already set up.
;;; This is because :PROCESS-SPEC may be recursively called on others'
;;; assertion lists if there are included modules with specs of their
;;; own.  In such case, we want to accept their attributes, but not
;;; their names.
;;;
;;; The ASSERTIONS are processed next, because presumably they specify
;;; prerequisites for this module and any files they need loaded should
;;; get set up before we set up the files particular to this module.
;;;
;;; Finally, the FILES associated with this module are processed.

(defmethod process-spec ((module module) s)
  (with-slots (name) module
    (when s
      (if (not name) (setq name (car s)))
      (process-assertions module (cddr s))
      (process-files module (cadr s)))))

;;; (PROCESS-FILES module files-list)
;;;
;;; Adds file info given in FILES-LIST to the module's master FILES list.

(defmethod process-files ((module module) files-list)
  (with-slots (source-files system) module
    (if (atom files-list) (setq files-list (list files-list)))
    (dolist (file files-list)
      (cond ((typep file 'pathname)
         (setq source-files (nconc source-files (list file))))
        ((stringp file)
         (setq source-files (nconc source-files (list (parse-namestring file)))))
        ((symbolp file)
         (process-spec system (module-spec (get-module system file))))
        (t
         (error "Bad object in file list: ~S - ~S" file system))))))

;;; (PROCESS-ASSERTIONS module spec)
;;;
;;; Iterates across assertions, processing each.

(defmethod process-assertions ((module module) assertion-list)
  (dolist (assertion assertion-list)
    (apply #'process-assertion (module-system module) assertion)))

;;; (PROCESS-ASSERTION module key . data)
;;;
;;; This method is used to process dependency assertions, etc.
;;; for the given module.
;;;
;;; It dispatches further on key:
;;;
;;;  :NEEDS     Declares need to instantiate modules at certain times.
;;;  :CAUSES    Declares assertions to be forwarded to the consumer.
;;;  Otherwise  it's an error.

(defmethod process-assertion ((module module) (key (eql :needs)) &rest data)
  (with-slots (needs) module
    (dolist (item data)
      (let ((marker (car item)))
        (dolist (module-name (cdr item))
          (when (not (member module-name (getf needs marker)))
            (let ((m (get-module (module-system module) module-name)))
              ;; This may be overly conservative, but will work...
              (process-assertions module (module-causes m))
              (setf (getf needs marker)
                    (nconc (getf needs marker) (list module-name))))))))))

(defmethod process-assertion ((module module) (key (eql :causes)) &rest data)
  ;; Filtering this is technically unnecessary, but it will keep
  ;; redefinition from swamping us.
  (with-slots (causes) module
    (dolist (item data)
      (if (not (member item causes :test #'equal))
          (setf causes (nconc causes (list item)))))))

(defmethod process-assertion ((module module) (key t) &rest data)
  (error "The ~S assertion is not known to ~S.~%Data: ~S"
         key module data))


;;;; User Interface

;;; (CREATE-SYSTEM name type [options])
;;;
;;; Creates a system object of the given TYPE, initializing it with
;;; the given NAME and OPTIONS. Returns the created object without
;;; storing it permanently anywhere.

(defun create-system (name class &optional options)
  (let ((system (make-instance class)))
    (process-option  system :name name)
    (process-options system options)
    system))

;;; (FIND-SYSTEM name)
;;;
;;; Gets the definition of some globally defined system object.

(defun find-system-named (system-name &optional (errorp t))
  (etypecase system-name
    (system system-name)
    (symbol (or (get system-name 'system)
                (if errorp
                    (error "System not found: ~S" system-name)
                  nil)))))

(defun (setf find-system-named) (new-system system-spec) 
  (let ((system-name (etypecase system-spec
                       (symbol system-spec)
                       (system (name system-spec)))))
    (setf (get system-name 'system) new-system)))

;;; (DEFINE-SYSTEM name type .  options)
;;;
;;; Creates and initializes a system with the given name.
;;; Stores the definition globally for access later.

(defmacro define-system (name type &body data)
  (check-type name symbol "a packaged symbol naming the system to be defined")
  `(setf (find-system-named ',name)
         (create-system ',name ',type ',data)))
