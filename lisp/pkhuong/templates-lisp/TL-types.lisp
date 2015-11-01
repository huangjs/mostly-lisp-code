(cl:in-package #:template-lisp)

(defclass argument ()
  ((name :accessor name
	 :initarg  :name
	 :type     (or symbol string)
	 :documentation "Name of the argument")
   (type :accessor arg-type
	 :initarg  :type
	 :type     (member typename class struct int bool)
	 :documentation "Type of the arg")
   (default :accessor default
            :initarg  :default
	    :type     (or null string number (member true false))
	    :initform ()
	    :documentation "Default value of the arg. Nil or the value (string, number, true/false)"))
  (:documentation
   "A template's non-specialised arguments."))

(defclass definition ()
  ((name :accessor name
	 :initarg  :name
	 :type     (or symbol string)
	 :documentation "Name of the defined value.")
   (type :accessor def-type
	 :initarg  :type
	 :type     (member template type struct class int bool raw)
	 :documentation "Type of the defined value (template, type [typedef], int, bool [static const], raw -> string).")
   (access :accessor access
	   :initarg  :access
	   :type     (member nil public private protected)
	   :initform '()
	   :documentation "Accessibility of the value. (Nil -> none. ***toplevel***)")
   (body :accessor body
	 :initarg  :body
	 :type     (or string template n-type)
	 :documentation "Body of the definition. (between typedef and name, = and ; or whole if raw)")))

(defclass template ()
  ((name :accessor name
	 :initarg  :name
	 :initform (symbol-name (gensym "template"))
	 :type (or symbol string)
	 :documentation "name of the template. *Case sensitive*")
   (args :accessor args
	 :initarg  :args
	 :initform ()
	 :type list
	 :documentation "arguments *of the template*.")
   (specialised-args
         :accessor spec-args
	 :initarg  :spec-args
	 :initform ()
	 :type     list
	 :documentation "specialised arguments, list of the value (string, number, true/false)"))
  (:documentation
   "Represents a template's definition.
It's only the bare essentials so as not to have
to parse C++ over and over again."))

(defclass t-type (template)
  ((parent :accessor parent
	   :initarg  :parent
	   :initform ()
	   :type (or null symbol string)
	   :documentation "NIL if it does not inherit, parent name's otherwise")
   (inherit-type
           :accessor inherit-type
	   :initarg  :inherit-type
	   :initform 'public
	   :type (member public private protected)
	   :documentation "How is the parent inherited from? 
Symbol from public, private or protected")
   (code   :accessor code
	   :initarg  :code
	   :initform ()
	   :type (or list string)
	   :documentation "Inside of the template. (typedef or static const)
Represented as a string or as a list of definitions/strings/templates"))
  (:documentation "A templated type (class, struct)"))

(defclass n-type ()
  ((name :accessor name
	 :initarg  :name
	 :type     (or symbol string)
	 :documentation "Name")
   (parent :accessor parent
	   :initarg :parent
	   :initform ()
	   :type     (or null symbol string))
   (inherit-type
           :accessor inherit-type
	   :initarg  :inherit-type
	   :initform 'public
	   :type     (member public private protected))
   (code   :accessor code
	   :initarg  :code
	   :initform ()
	   :type     (or list string))))

(defclass t-class (t-type)
  ()
  (:documentation "A templated class."))

(defclass t-struct (t-type)
  ()
  (:documentation "A templated struct."))

(defclass n-class (n-type)
  ())
(defclass n-struct (n-type)
  ())

(defclass function-arg ()
  ((name :accessor name
	 :initarg  :name
	 :type     (or string symbol)
	 :documentation "Name of the arg. string or symbol")
   (arg-type :accessor arg-type
	 :initarg  :arg-type
	 :type     string
	 :documentation "Type of the arg. String")))

(defclass t-function (template)
  ((return-type :accessor return-type
		:initarg  :return-type
		:type     string
		:documentation "Return type of the function (string).")
   (function-args :accessor function-args
		  :initarg  :function-args
		  :initform ()
		  :type     list
		  :documentation "Arguments of the function. List of arguments (see function-arg)")
   (body :accessor body
	 :initarg  :body
	 :initform ()
	 :type     string
	 :documentation "Body of the function (string).")))

;;TODO: define a non-templated function type too.