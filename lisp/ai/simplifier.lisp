;;; infix to prefix convertion

;;; examples:
;;; (((a * (x ^ 2) + (b * x)) + c)  make fully use of parens
;;; (a * x ^ 2 + b * x + c)   make use of operator precedence
;;; (a x ^ 2 + b x + c)   make use of implicit multiplication as well as op precedence
;;; a x^2 + b*x+c   requires a lexical analyzer to break LISP symbols into pieces.
;;; one important fact: infix->prefix equals prefix->infix!!!
(defun infix->prefix (infix-exp)
  "Convert fully parenthesized infix-exp to a prefix expression"
  ;; Don't use this version for non-fully parenthesized exps!
  (prefix->infix infix-exp))

(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation.
Can use operator precedence."
  ;; Note we cannot do implicit multiplication in this system.
  (cond ((atom exp) exp)                ; 3
        ((= (length exp) 1) (infix->prefix (first exp)))  ; (3)
        ((rule-based-translator exp *infix->prefix->rules*
                                :rule-if #'rule-pattern
                                :rule-then #'rule-response
                                :action #'(lambda (bindings response)
                                            (sublis (mapcar
                                                     #'(lambda (pair)
                                                         (cons (first pair)
                                                               (infix->prefix (rest pair))))
                                                     bindings)
                                                    response))))
        ((symbolp (first exp))
         (list (first exp) (infix->prefix (rest exp))))
        (t (error "Illegal exp"))))

(defun variable? (exp)
  "Variables are the symbols M through Z."
  ;; put x,y,z first to find them a little faster
  (member exp '(x y z m n o p q r s t u v w)))

(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

(defun rule-pattern (rule) (first rule))
(defun rule-response (rule) (second rule))

(defparameter *infix->prefix->rules*
  (mapcar #'expand-pat-match-abbrev
          '(((x+ = y+) (= x y))
            ((- x+) (- x))
            ((+ x+) (+ x))
            ((x+ + y+) (+ x y))
            ((x+ - y+) (- x y))
            ((x+ * y+) (* x y))
            ((x+ / y+) (/ x y))
            ((x+ ^ y+) (^ x y))))
  "A list of rules, ordered by precedence.")


;;; simplification rules
(defstruct (rule (:type list)) pattern response)
(defstruct (exp (:type list)
                (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun exp? (x) (consp x))
(defun exp-args (x) (rest x))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar #'prefix->infix
              (if (binary-exp? exp)
                  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                  exp))))

(defun binary-exp? (x)
  (and (exp? x) (= (length (exp-args x)) 2)))

(defparameter *simplification-rules*
  (mapcar #'infix->prefix
          '((x + 0 = x)
            (0 + x = x)
            (x + x = 2 * x)
            (x - 0 = x)
            (0 - x = - x)
            (x - x = 0)
            (- - x = x)
            (x * 1 = x)
            (1 * x = x)
            (x * 0 = 0)
            (0 * x = 0)
            (x * x = x ^ 2)
            (x / 0 = undefined)         ;note the sequence of the rules
            (0 / x = 0)
            (x / 1 = x)
            (x / x = 1)
            (0 ^ 0 = undefined)
            (x ^ 0 = 1)
            (0 ^ x = 0)
            (1 ^ x = 1)
            (x ^ 1 = x)
            (x ^ -1 = 1 / x)
            (x * (y / x) = y)
            ((y / x) * x = y)
            ((y * x) / x = y)
            ((x * y) / x = y)
            (x + - x = 0)
            ((- x) + x = 0)
            (x + y - x = y)
            )))

(defun ^ (x y) "Exponentiation" (expt x y))

(defun simplifier ()
  "Read a mathematical expression , simplify it, and print the result."
  (loop
      (print 'simplifier>)
      (print (simp (read)))))

(defun simp (inf) (prefix->infix (simplify (infix->prefix inf))))

(defun simplify (exp)
  "Simplify an expression by first simplifying its components."
  (if (atom exp)
      exp
      (simplify-exp (mapcar #'simplify exp))))

(defun simplify-exp (exp)
  "Simplify using a rule, or by doing arithmethic."
  (cond ((rule-based-translator exp *simplification-rules*
                                :rule-if #'exp-lhs
                                :rule-then #'exp-rhs
                                :action #'(lambda (bindings response)
                                            (simplify (sublis bindings response)))))
        ((evaluable? exp) (eval exp))
        (t exp)))

(defun evaluable? (exp)
  "Is this an arithmetic expression that can be evaluated?"
  (and (every #'numberp (exp-args exp))
       (or (member (exp-op exp) '(+ - * /))
           (and (eq (exp-op exp) '^)
                (integerp (second (exp-args exp)))))))


;;; associativity and commutativity
;;; scenario:
;;; (3 * (2 * x)) -> ((3 * 2) * x) -> ( 6 * x)
;;; ((2 * x) * (3 * 6)) -> (6 * (x * y))
;;; (((?is n numberp) * ((?is m numberp) * x)) = ((n * m) * x)) doesn't work
;;; we need an ALGORITHM to achieve this

;; Define n and m as numbers; s as a non-number:
(pat-match-abbrev 'n '(?is n numberp))
(pat-match-abbrev 'm '(?is m numberp))
(pat-match-abbrev 's '(?is s not-numberp))

(defun not-numberp (x) (not (numberp x)))

(defun simp-rule (rule)
  "Transform a rule into proper format."
  (let ((exp (infix->prefix rule)))
    (mkexp (expand-pat-match-abbrev (exp-lhs exp))
           (exp-op exp) (exp-rhs exp))))

;;; run once
(setf *simplification-rules*
      (append *simplification-rules*
              (mapcar #'simp-rule
                      '((s * n = n * s)
                        (n * (m * x)  = (n * m) * x)
                        (x * (n * y) = n * (x * y))
                        ((n * x) * y = n * (x * y))
                        (n + s = s + n)
                        ((x + m) + n = x + n + m)
                        (x + (y + n) = (x + y) + n)
                        ((x + n) + y = (x + y) + n)))))
                                             
;;; log, trig, 
(setf *simplification-rules*
     (append *simplification-rules* (mapcar #'simp-rule
       '((log 1 = 0)
         (log 0 = undefined)
         (log e = 1)
         (sin 0 = 0)
         (sin pi  = 0)
         (cos 0 = 1)
         (cos pi = -1)
         (sin(pi / 2) = 1)
         (cos(pi / 2) = 0)
         (log(e ^ x) = x)
         (e ^ (log x) = x)
         ((x ^ y) * (x ^ z) = x ^ (y + z))
         ((x ^ y) / (x ^ z) = x ^ (y - z))
         (log x + log y = log(x * y))
         (log x - log y = log(x / y))
         ((sin x) ^ 2 + (cos x) ^ 2 = 1)
         ))))

;;; differentiation
(defparameter *infix->prefix->rules*
  (mapcar #'expand-pat-match-abbrev
          '(((x+ = y+) (= x y))
            ((- x+) (- x))
            ((+ x+) (+ x))
            ((x+ + y+) (+ x y))
            ((x+ - y+) (- x y))
            ;; diff rules before division rules, more specific
            ((d y+ / d x) (d y x))      ; *** new rule
            ((int y+ d x) (int y x))    ; *** new rule
            ((x+ * y+) (* x y))
            ((x+ / y+) (/ x y))
            ((x+ ^ y+) (^ x y))))
  "A list of rules, ordered by precedence.")

(setf *simplification-rules*
      (append *simplification-rules* (mapcar #'simp-rule
       '((d x / d x = 1)
         (d (u + v) / d x = (d u / d x) + (d v / d x))
         (d (u - v) / d x = (d u / d x) - (d v / d x))
         (d (- u) / d x = - (d u / d x))
         (d (u * v) / d x = u * (d v / d x) + v * (d u / d x))
         (d (u / v) / d x = (v * (d u / d x) - u * (d v / d x)) / v ^ 2)
         (d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))
         (d (u ^ v) / d x = v * u ^ (v - 1) * (d u / d x)
                          + u ^ v * (log u) * (d v / d x))
         (d (log u) / d x = (d u / d x) / u)
         (d (sin u) / d x = (cos u) * (d u / d x))
         (d (cos u) / d x = - (sin u) * (d u / d x))
         (d (e ^ u) / d x = (e ^ u) * (d u / d x))
         (d u / d x = 0)))))

