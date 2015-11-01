;;; syntax:
;;; (computed-goto [switch expression] [expression for case 0] [... 1] ...)

;; The computed goto feature consists of two parts,
;; one portable, the other not, but much smaller, (as
;; with all low level extensions in SBCL).

(cl:in-package "SB-VM")
;; This vop takes as runtime argument the jump index
;; and as compile time argument (:info slot) a list
;; of (block-)labels, the last of which may be nil to
;; indicate a fall through.
(define-vop (sb-c::computed-goto)
  (:temporary (:sc unsigned-reg) jump-target)
  (:temporary (:sc any-reg) count-temp)
  (:args (count :scs (any-reg) :load-if t :target count-temp))
  (:info targets)
  (:generator 10
     (aver targets)
     (aver (every 'identity (butlast targets)))
     (move count-temp count)
     (inst lea jump-target (make-fixup nil :code-object HERE))
     (inst shr count-temp 1)
     (inst lea jump-target (make-ea :qword
                                    :base  jump-target
                                    :index count-temp))
     (inst jmp jump-target)
     (align 2 #x90) ; This is wrong. Should check the actual
     HERE           ; size of the jumps (or even encode only
                    ; the addresses/deltas!) to find alignment.
      (dolist (target targets)
        (align 2 #x90)
        (when target
          (inst jmp target)))))

(cl:in-package "SB-C")
;; Define a new ctran node to represend computed gotos.
(defstruct (ccomputed-goto (:include node)
                           (:conc-name cgoto-)
                           (:predicate cgoto-p)
                           (:constructor make-cgoto)
                           (:copier copy-cgoto))
  (index (missing-arg) :type lvar)
  (cases (missing-arg) :type list))

;; Half cargo-culted from if
(def-ir1-translator computed-goto ((index &body cases) start next result)
  (let* ((index-ctran (make-ctran))
         (index-lvar  (make-lvar))
         (case-ctrans (mapcar (lambda (x)
                                (declare (ignore x))
                                (make-ctran))
                              cases))
         (case-blocks (mapcar (lambda (ctran)
                                (ctran-starts-block ctran))
                              case-ctrans))
         (node         (make-cgoto :index index-lvar
                                   :cases case-blocks)))
    (setf (lvar-dest index-lvar) node)
    (ir1-convert start index-ctran index-lvar index)
    (link-node-to-previous-ctran node index-ctran)
          
    (let ((start-block (ctran-block index-ctran)))
      (setf (block-last start-block) node)
      (ctran-starts-block next)
      (dolist (block case-blocks)
        (link-blocks start-block block)))

    (mapcar (lambda (case ctran)
              (ir1-convert ctran next result case))
            cases case-ctrans)
    (values)))

(defun ltn-analyze-cgoto (node)
  (declare (type ccomputed-goto node))
  (setf (node-tail-p node) nil)
  (let ((index (cgoto-index node)))
    (annotate-ordinary-lvar index))
  (values))

(defun ir2-convert-cgoto (node block)
  (declare (type ir2-block block)
           (type ccomputed-goto node))
  (let* ((index (cgoto-index node))
         (index-ref (reference-tn (lvar-tn node block index) nil))
         (labels    (mapcar 'block-label
                            (cgoto-cases node))))
    (aver (csubtypep (lvar-type index)
                     (make-numeric-type :class 'integer
                                        :low 0
                                        :high (1- (length labels)))))
    (when (drop-thru-p node (first (last (cgoto-cases node))))
      (setf (car (last labels)) nil))
    (emit-template node block (template-or-lose 'computed-goto)
                   index-ref nil (list labels))))

;; Must add a case for ccomputed-goto.
;; Would it really be that bad to start using
;; CLOS for that?
(defun ltn-analyze-block (block)
  (do* ((node (block-start-node block)
              (ctran-next ctran))
        (ctran (node-next node) (node-next node)))
      (nil)
    (etypecase node
      (ref)
      (combination
       (ecase (basic-combination-kind node)
         (:local (ltn-analyze-local-call node))
         ((:full :error) (ltn-default-call node))
         (:known
          (ltn-analyze-known-call node))))
      (cif (ltn-analyze-if node))
      (ccomputed-goto (ltn-analyze-cgoto node))
      (creturn (ltn-analyze-return node))
      ((or bind entry))
      (exit (ltn-analyze-exit node))
      (cset (ltn-analyze-set node))
      (cast (ltn-analyze-cast node))
      (mv-combination
       (ecase (basic-combination-kind node)
         (:local
          (ltn-analyze-mv-bind node))
         ((:full :error)
          (ltn-analyze-mv-call node)))))
    (when (eq node (block-last block))
      (return))))

;; Special case computed-goto nodes
;; like if ones (allow multiple successors)
(defun finish-ir2-block (block)
  (declare (type cblock block))
  (let* ((2block (block-info block))
         (last (block-last block))
         (succ (block-succ block)))
    (unless (or (if-p last)
                (cgoto-p last))
      (aver (singleton-p succ))
      (let ((target (first succ)))
        (cond ((eq target (component-tail (block-component block)))
               (when (and (basic-combination-p last)
                          (eq (basic-combination-kind last) :full))
                 (let* ((fun (basic-combination-fun last))
                        (use (lvar-uses fun))
                        (name (and (ref-p use)
                                   (leaf-has-source-name-p (ref-leaf use))
                                   (leaf-source-name (ref-leaf use)))))
                   (unless (or (node-tail-p last)
                               (info :function :info name)
                               (policy last (zerop safety)))
                     (vop nil-fun-returned-error last 2block
                          (if name
                              (emit-constant name)
                              (multiple-value-bind (tn named)
                                  (fun-lvar-tn last 2block fun)
                                (aver (not named))
                                tn)))))))
              ((not (eq (ir2-block-next 2block) (block-info target)))
               (vop branch last 2block (block-label target)))))))

  (values))

;;; Convert the code in a block into VOPs.
;; Again, add a case for computed-goto,
;; copy-pasted from if.
(defun ir2-convert-block (block)
  (declare (type cblock block))
  (let ((2block (block-info block)))
    (do-nodes (node lvar block)
      (etypecase node
        (ref
         (when lvar
           (let ((2lvar (lvar-info lvar)))
             ;; function REF in a local call is not annotated
             (when (and 2lvar (not (eq (ir2-lvar-kind 2lvar) :delayed)))
               (ir2-convert-ref node 2block)))))
        (combination
         (let ((kind (basic-combination-kind node)))
           (ecase kind
             (:local
              (ir2-convert-local-call node 2block))
             (:full
              (ir2-convert-full-call node 2block))
             (:known
              (let* ((info (basic-combination-fun-info node))
                     (fun (fun-info-ir2-convert info)))
                (cond (fun
                       (funcall fun node 2block))
                      ((eq (basic-combination-info node) :full)
                       (ir2-convert-full-call node 2block))
                      (t
                       (ir2-convert-template node 2block))))))))
        (cif
         (when (lvar-info (if-test node))
           (ir2-convert-if node 2block)))
        (ccomputed-goto
         (when (lvar-info (cgoto-index node)) ; WHY?
           (ir2-convert-cgoto node 2block)))
        (bind
         (let ((fun (bind-lambda node)))
           (when (eq (lambda-home fun) fun)
             (ir2-convert-bind node 2block))))
        (creturn
         (ir2-convert-return node 2block))
        (cset
         (ir2-convert-set node 2block))
        (cast
         (ir2-convert-cast node 2block))
        (mv-combination
         (cond
           ((eq (basic-combination-kind node) :local)
            (ir2-convert-mv-bind node 2block))
           ((eq (lvar-fun-name (basic-combination-fun node))
                '%throw)
            (ir2-convert-throw node 2block))
           (t
            (ir2-convert-mv-call node 2block))))
        (exit
         (when (exit-entry node)
           (ir2-convert-exit node 2block)))
        (entry
         (ir2-convert-entry node 2block)))))

  (finish-ir2-block block)

  (values))
