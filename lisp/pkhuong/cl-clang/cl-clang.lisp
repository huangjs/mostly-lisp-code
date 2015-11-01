(in-package "CL-CLANG")

(defmacro missing-arg (&optional name)
  (if name
      `(error "Missing arg ~S" ',name)
      `(error "Missing arg")))
(defun boolify (x)
  (if x 1 0))

(defstruct (index
             (:constructor %make-index (%index)))
  (%index (missing-arg) :type (alien clang-ffi:index)
   :read-only t))

(defun make-index (&key
                   (exclude-declarations-from-pch t)
                   (display-diagnostics t))
  (let* ((%index (clang-ffi:create-index (boolify exclude-declarations-from-pch)
                                         (boolify display-diagnostics)))
         (index  (%make-index %index)))
    (finalize index (lambda ()
                      (clang-ffi:dispose-index %index)))
    index))

(defstruct (translation-unit
             (:constructor %%make-translation-unit (%translation-unit index)))
  (%translation-unit (missing-arg) :type (alien clang-ffi:translation-unit)
   :read-only t)
  (index (missing-arg) :type index))

(defun %make-translation-unit (%tu index)
  (declare (type (alien clang-ffi:translation-unit) %tu)
           (type index index))
  (let ((tu (%%make-translation-unit %tu index)))
    (finalize tu (lambda ()
                   (clang-ffi:dispose-translation-unit %tu)))))

(defun make-translation-unit-from-source (index source
                                          &key (command-line-args '())
                                               (unsaved-files '()))
  (assert (null unsaved-files))
  (assert (every #'stringp command-line-args))
  (let ((num-args (length command-line-args)))
    (with-alien ((args (* c-string) (make-alien c-string num-args)))
      (loop for i upfrom 0
            for arg in command-line-args
            do
         (setf (deref args i) (make-alien-string arg)))
      (prog1 (%make-translation-unit (clang-ffi:create-translation-unit-from-source-file
                                      (index-%index index)
                                      source
                                      num-args args
                                      0 nil)
                                     index)
        (loop for i below num-args
              do
           (free-alien (deref args i)))
        (free-alien args)))))

(defstruct cursor
  (%cursor (missing-arg) :type (alien clang-ffi:cursor)
   :read-only t)
  (%hash -1 :type fixnum)
  (kind nil)
  (linkage nil)
  (availability nil)
  (language nil)
  (child-nodes nil)
  (children nil))

(defstruct source-location
  (%location (missing-arg) :type (alien clang-ffi:source-location)
   :read-only t))
(defstruct source-range
  (%range (missing-arg) :type (alien clang-ffi:source-range)
   :read-only t))
(defstruct ctype
  (%type  (missing-arg) :type (alien clang-ffi:type)
   :read-only t))
(defstruct token
  (%token (missing-arg) :type (alien clang-ffi:token)
   :read-only t))

(defun ffi-string-string (string)
  (declare (type (alien clang-ffi:string) string))
  (prog1 (clang-ffi:get-string string)
    (clang-ffi:dispose-string string)))

