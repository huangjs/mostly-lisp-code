;;; simple pattern matching

(defun match (pat inp)
  "?v is a varaible"
  (matchb pat inp '((t .t))))

;;; return bindings or nil when match fails
(defun matchb (pat inp bindings)
  (and bindings
       (if (consp pat)
           (and (consp inp)
                (matchb (cdr pat)
                        (cdr inp)
                        (matchb (car pat)
                                (car inp)
                                bindings)))
           (if (varp pat)
               (let ((binding (assoc pat bindings)))
                 (if binding
                     (and (equal inp (cdr binding)) bindings)
                     (cons (cons pat inp) bindings)))
               (and (eql pat inp) bindings)))))

(defun varp (v)
  (and (symbolp v)
       (char= #\? (char (symbol-name v) 0))))

(defun transf (rule input)
  "rule is of form (from to &optional test new-bindings)"
  (let ((bindings (match (first rule) input))
        (test (third rule)))
    (if (and bindings
             (or (null test)
                 (eval (sublisq bindings test))))
        (progn
          (dolist (var (fourth rule))
            (push (cons (car var)
                        (eval (sublisq bindings (cadr var))))
                  bindings))
          (sublis bindings (second rule)))
        'match-failure)))

;;; sublis, quoting value of bindings
(defun sublisq (bindings form)
  (sublis
   (mapcar #'(lambda (x)
               (cons (car x) (kwote (cdr x))))
           bindings)
   form))

(defun kwote (x)
  (if (constantp x)
      x
      (list 'quote x)))

;;; try to match input against optimization patterns
(defvar *compile-time-constants* nil)

(defun ptmatch (inp patwd)
  (let (patterns pattern tmp (res 'match-failure))
    (if (setf tmp (assoc inp *compile-time-constants* :test 'equal))
        (cadr tmp)
        (if (and (consp inp)
                 (symbolp (car inp))
                 (setf patterns
                       (get (car inp) patwd)))
            (progn
              (loop while (and patterns
                               (eq res 'match-failure))
                    do (setf res
                             (transf (pop patterns) inp)))
              (if (eq res 'match-failure)
                  inp
                  res))
            inp))))

(defun trans (x patwd)
  (let (xp tail tmp)
    (if (consp x)
        (if (and (not (member (first x)
                              '(print princ terpri format random)))
                 (rest x)
                 (symbolp (first x))
                 (fboundp (first x))
                 (every #'constantp (rest x)))
            (eval x)
            (progn                      ; translate args first
              (setq tail (pttransl (rest x) patwd))
              (if (symbolp (first x))
                  (setq xp (ptmatch
                            (if (eq tail (rest x))
                                x
                                (cons (first x) tail))
                            patwd))
                  (progn (setq tmp (trans (first x) patwd))
                         (setq xp (if (and (eq tail (rest x))
                                           (eql tmp (first x)))
                                      x
                                      (cons tmp tail)))))
              (if (eq x xp) x (trans xp patwd))))
        (if (and (symbolp x)
                 (setq tmp (assoc x *compile-time-constants*)))
            (cadr tmp)
            x))))


;; Print stuff out in C form
(defun cpr (item optional tabs)
  (let (newtabs)
    (if (stringp item)
        (princ item)
        (if (characterp item)
            (if (char= item #\Return)
                (progn (terpri)
                       (if tabs (spaces tabs)))
                (princ item))
            (if (symbolp item)
                (princ (string-downcase (symbol-name item)))
                (if (consp item)
                    (if (symbolp (first item))
                        (progn (cpr (first item))
                               (princ "(")
                               (mapl
                                #'(lambda (z)
                                    (if (not (eq z (rest item)))
                                        (princ ", "))
                                    (cpr (car z)))
                                (rest item))
                               (princ ")"))                             
                        (dolist (z item)
                          (if (and (characterp z) (char= z #\Tab))
                              (setq newtabs (+ (or tabs 0) 2))
                              (cpr z (or newtabs tabs)) )))
                    (princ item)))))))

#| test

(match '(- (+ ?x ?y) (+ ?z ?y))
  '(- (+ (age tom) (age mary))
    (+ (age bill) (age mary))))

(transf '((- (+ ?x ?y) (+ ?z ?y))
             (- ?x ?z))
           '(- (+ (age tom) (age mary))
             (+ (age bill) (age mary))))

;;; it is possible to do dot matching
(transf '((progn nil . ?s) (progn . ?s))
           '(progn nil (setq x 3) (setq y 7)))

;;; predicate and introducing new bindings
(transf '((intersection
           (subset (function (lambda (?x) ?p))
            ?s)
           (subset (function (lambda (?y) ?q))
            ?s))
          (subset (function (lambda (?x)
                    (and ?p ?qq)))
           ?s)
          t
          ((?qq (subst ?x ?y ?q))))
        '(intersection
          (subset #'(lambda (w) (rich w)) people)
          (subset #'(lambda (z) (famous z)) people)))

;;; problematic rules, there're false cases

( (>  (* ?n ?x)
      (* ?n ?y))         (>  ?x ?y) )

( (not (not ?x))        ?x )

( (eq (if ?p ?qu ?qv)
      ?qu)              ?p )

( (rplaca ?x ?y)        (setf (car ?x) ?y) )


|#

