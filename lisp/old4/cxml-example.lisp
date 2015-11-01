(defun node-parent (node) ;; FIXME: maybe not quite correct
  (if (dom:attribute-p node)
      (dom:owner-element node)
      (dom:parent-node node)))

(defgeneric compare-xml (a b)
  (:documentation "Compare two XML nodes"))

(defmethod compare-xml (a b) (list a b))

(defmethod compare-xml ((a dom:document) (b dom:document))
  (compare-xml (dom:document-element a) (dom:document-element b)))

(defun get-attribute-names (element)
  (let ((result '()))
    (dom:do-node-map (attr (dom:attributes element))
      (unless (has-prefix-p (dom:node-name attr))
        (push (dom:node-name attr) result)))
    (nreverse result)))

(defun should-skip-node-p (node)
  (or (dom:processing-instruction-p node)
      (dom:comment-p node)
      (dom:document-type-p node)
      (and (dom:character-data-p node)
           (cl-ppcre:scan (rx "^\\s*$" :single-line-mode t) (dom:node-value node)))))

(defun compare-children (a b)
  (cond ((and (null a) (null b)) nil)
        ((null a) (list nil (first b)))
        ((null b) (list (first a) nil))
        ((should-skip-node-p (first a))
         (compare-children (rest a) b))
        ((should-skip-node-p (first b))
         (compare-children a (rest b)))
        (t (or (compare-xml (first a) (first b))
               (compare-children (rest a) (rest b))))))

(defmethod compare-xml ((a dom:element) (b dom:element))
  (cond ((string/= (dom:node-name a) (dom:node-name b)) (list a b))
        ((loop for attr-name in (union (get-attribute-names a)
                                       (get-attribute-names b))
	    for diff = (compare-xml (dom:get-attribute-node a attr-name)
				    (dom:get-attribute-node b attr-name))
	    when diff do (return diff)))
        (t (compare-children (node-list->list (dom:child-nodes a))
                             (node-list->list (dom:child-nodes b))))))

(defun compare-node-values (a b)
  (when (string/= (trim (dom:node-value a)) (trim (dom:node-value b)))
    (list a b)))

(defmethod compare-xml ((a dom:attr) (b dom:attr)) (compare-node-values a b))

(defmethod compare-xml ((a dom:text) (b dom:text)) (compare-node-values a b))

(defmethod compare-xml ((a dom:cdata-section) (b dom:cdata-section)) (compare-node-values a b))

;; FIXME: entity references?

(defun null-if-empty (attr)
  (if (zerop (length attr))
      nil
      t))

(defun simple-path-place (node)
  (typecase node
    (dom:document "[document]")
    (dom:element (let ((id (or (null-if-empty (dom:get-attribute node "id"))
                               (null-if-empty (dom:get-attribute node "Id")))))
                   (if id
                       (format nil "~a[@id='~a']" (dom:node-name node) id)
                       (dom:node-name node))))
    (dom:attr (s+ "@" (dom:node-name node)))
    (dom:text (format nil "text(~s)" (dom:node-value node)))
    (dom:cdata-section (format nil "text(~s)" (dom:node-value node)))
    (t (format nil "[~s]" (type-of node)))))

(defun simple-path (node)
  (cond ((null node) nil)
        ((null (node-parent node))
         (simple-path-place node))
        (t
         (s+ (simple-path (node-parent node))
	     "/" (simple-path-place node)))))
