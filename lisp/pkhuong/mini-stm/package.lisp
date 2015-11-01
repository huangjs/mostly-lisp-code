(defpackage "STM"
    (:use "CL" "SB-EXT" "SB-SYS" "SB-ALIEN")
  (:export #:call-with-transaction #:with-transaction
           #:**aborts**
           #:*transaction-init-hooks*
           #:*transaction-rollback-hooks*
           #:*transaction-end-hooks*

           #:action #:action-function
           #:+last-action+ #:abort-transaction
           #:commit #:rollback #:release
           
           #:with-transaction-data

           #:thread-id #:in-transaction-p #:irrevocable-p
           #:%thread-id% #:%actions% #:%locks% #:%transaction-id%
           #:define-thread-local-variable
           #:definline-macro

           #:bytelock #:make-bytelock #:bytelock-cleaner
           #:bytelock-read #:bytelock-write #:release-bytelock

           #:cell #:locked-cell #:rw-cell
           #:make-cell #:cell-value))
