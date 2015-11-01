(defun expt (x)
  (cond ((= 0 x) (out struct result ()
		      "template <typename T>"
		      "static inline T expt(T y) { return 1; }"))
	((= 1 x) (out struct result ()
		      "template <typename T>"
		      "static inline T expt(T y) { return y; }"))
	(T       (out struct result ()
		      "template <typename T>"
		      "static inline T expt(T y)" 
		      "{"
		      "    return #{ (expt (/ x 2)) }::expt(y) *"
		      "           #{ (expt (/ (+ 1 x) 2)) }::expt(y);"
		      "}"))))
