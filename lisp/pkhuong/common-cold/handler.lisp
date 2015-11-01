(cl:in-package "COMMON-COLD")

(defvar *key* nil
  "Key used for symmetric encryption (AES) of continuations
NIL -> no encryption")

(defun register-key (key)
  (setf *key*
        (let* ((octets (sb-ext:string-to-octets key))
               (length (length octets)))
          (and (> length 0)
               (flet ((pad-to (n)
                        "Is that a good idea, or should I just 0-pad?"
                        (apply 'concatenate
                               '(vector (unsigned-byte 8))
                               (subseq octets 0 (rem n length))
                               (make-list (floor n length)
                                          :initial-element octets))))
                 (ironclad:make-cipher :aes
                                       :key  (cond ((<= length 16) (pad-to 16))
                                                   ((<= length 24) (pad-to 23))
                                                   ((<= length 32) (pad-to 32))
                                                   (t (error "Maximum key size for AES: 32 bytes")))
                                       :mode :ecb))))))

(defun compress (string)
  (zlib:compress (sb-ext:string-to-octets string) :fixed))

(defun inflate (ub8s)
  (sb-ext:octets-to-string (zlib:uncompress ub8s)))

(defun word-to-octets (x)
  (declare (type (unsigned-byte 32) x))
  (make-array 4
              :element-type '(unsigned-byte 8)
              :initial-contents (list (ldb (byte 8 0) x)
                                      (ldb (byte 8 8) x)
                                      (ldb (byte 8 16) x)
                                      (ldb (byte 8 24) x))))

(defun octets-to-word (octets)
  (+ (* (expt 2 0)  (aref octets 0))
     (* (expt 2 8)  (aref octets 1))
     (* (expt 2 16) (aref octets 2))
     (* (expt 2 24) (aref octets 3))))

(defun encrypt (octets)
  (if (null *key*)
      octets
      (let* ((octets (concatenate '(vector (unsigned-byte 8))
                                  (word-to-octets (length octets))
                                  octets))
             (octets (concatenate '(vector (unsigned-byte 8))
                                  octets
                                  (make-array (- (* 16 (ceiling (length octets) 16))
                                                 (length octets))
                                              :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
             (out    (make-array (* 2 (length octets))
                                 :element-type '(unsigned-byte 8)))
             (length (nth-value 1
                                (ironclad:encrypt *key*
                                                  octets
                                                  out))))
        (subseq out 0 length))))

(defun decrypt (octets)
  (if (null *key*)
      octets
      (let* ((out (make-array (* 2 (length octets))
                              :element-type '(unsigned-byte 8)
                              :initial-element 0))
             (length (nth-value 1
                                (ironclad:decrypt *key* octets out)))
             (out    (subseq out 0 length)))
        (subseq out 4 (+ 4 (octets-to-word out))))))

(defun replace-bindings (cont bindings)
  "Replaces the value of the toplevel bindings
in cont with those in the bindings alist"
  (labels ((inner (cont bindings)
             (if (null cont)
                 (values nil bindings)
                 (destructuring-bind (frame . cont)
                     cont
                   (if (typep frame 'dynamic-binding)
                       (multiple-value-bind (cont bindings)
                           (inner cont bindings)
                         (let ((entry (assoc (var-of frame)
                                             bindings)))
                           (if entry
                               (values (cons (dyn-bind (var-of frame)
                                                       (cdr entry))
                                             cont)
                                       (remove (var-of frame) bindings
                                               :key 'car))
                               (values (cons frame cont)
                                       bindings))))
                       (multiple-value-bind (cont bindings)
                           (inner cont bindings)
                         (values (cons frame cont)
                                 bindings)))))))
    (inner cont bindings)))

(defun find-toplevel-bindings (cont)
  (reduce (lambda (frame bindings)
            (if (and (typep frame 'dynamic-binding)
                     (not (assoc (var-of frame) bindings)))
                (acons (var-of frame)
                       (val-of frame)
                       bindings)
                bindings))
          cont
          :from-end t
          :initial-value '()))

(defun cont-to-str (cont)
  (let ((*package* (find-package "KEYWORD"))
        (*print-circle* t))
    (format nil "~A?~{~A&~}"
            (cl-base64:usb8-array-to-base64-string
             (encrypt
              (compress
               (prin1-to-string cont)))
             :uri t)
            (let ((toplevel-bindings
                   (find-toplevel-bindings cont)))
              (mapcar (lambda (binding)
                        (format nil "~A=~A"
                                (url-encode (prin1-to-string (car binding)))
                                (url-encode (prin1-to-string (cdr binding)))))
               toplevel-bindings)))))

(defun call-with-continuation-url (fun) ; misnomer...
  (throw 'capture
    (list
     (lambda (cont)
       (let ((str-cont (cont-to-str cont))
             (bindings (find-toplevel-bindings cont)))
         (progv (mapcar 'car bindings)
             (mapcar 'cdr bindings)
           (funcall fun str-cont)))))))

(defmacro send/suspend ((cont-str-var) &body body)
  `(call-with-continuation-url (lambda (,cont-str-var)
                                 ,@body)))

(defun setup-capture (thunk)
  (block nil
    (let* ((cont (catch 'capture
                   (return (funcall thunk))))
           (handler (first (last cont)))
           (cont    (butlast cont)))
      (funcall handler (prune-redundant-frames cont)))))

(defun /trace (str val &rest args)
  (format t str val args)
  val)

(defun make-continuation-handler (default
                                  defaultp
                                  &optional (on-error
                                             (lambda (error)
                                               (format t "error:~%~A~%" error)
                                               (force-output)
                                               (cl-who:with-html-output-to-string (*standard-output*)
                                                 (:html (:head (:title "Oups"))
                                                        (:body "Someone messed up."))))))
  (let ((defaultp (if (or (symbolp defaultp)
                          (functionp defaultp))
                      defaultp
                      (lambda (name)
                        (string= name defaultp)))))
    (lambda ()
      (let ((b64-cont (first (last (split-sequence #\/
                                                   (script-name)
                                                   :remove-empty-subseqs t))))
            (params   (mapcar (lambda (pair)
                                (let ((*read-eval* nil))
                                  (cons (read-from-string (car pair) nil)
                                        (read-from-string (cdr pair) nil))))
                              (reverse (get-parameters)))))
        (multiple-value-bind (out error)
            (ignore-errors
              (if (and b64-cont
                       (not (funcall defaultp (script-name))))
                  (let ((cont (read-from-string
                               (inflate
                                (decrypt
                                 (base64-string-to-usb8-array b64-cont
                                                              :uri t)))
                               nil)))
                    (setup-capture
                     (lambda ()
                       (invoke-cont (replace-bindings cont params)
                                    :value params))))
                  (setup-capture default)))
          (if (stringp out)
              out
              (funcall on-error error)))))))
