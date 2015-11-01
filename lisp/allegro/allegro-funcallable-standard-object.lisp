(unless (mop:class-finalized-p (find-class 'standard-object))
  (mop:finalize-inheritance (find-class 'standard-object)))

(unless (mop:class-finalized-p (find-class 'mop:funcallable-standard-object))
  (mop:finalize-inheritance (find-class 'mop:funcallable-standard-object)))

(defclass test (mop:funcallable-standard-object)
  ()
  (:metaclass mop:funcallable-standard-class))

