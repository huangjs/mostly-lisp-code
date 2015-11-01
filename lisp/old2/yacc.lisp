;;; report line number and column number in yacc.

(define-condition yacc-parse-error-with-position (yacc-parse-error)
  ((stream-line-number :initarg :stream-line-number
                       :reader yacc-parse-error-with-position-line-number)
   (stream-column-number :initarg :stream-column-number
                         :reader
                         yacc-parse-error-with-position-column-number))
  (:report (lambda (e stream)
			 (format stream "~<Unexpected terminal ~S (value ~S)~
                            Expected one of: ~S~
                            Line number ~D~
                            Column number ~D~@:>"
                     (yacc-parse-error-terminal e)
                     (yacc-parse-error-value e)
                     (yacc-parse-error-expected-terminals e)
                     (yacc-parse-error-with-position-line-number e)
                     (yacc-parse-error-with-position-column-number e)))))

;; *STREAM* is your real input stream, *LEXER* is your lexer function,
;; *PARSER* is your parser.
(let ((counting-stream
       (make-instance 'counting-character-input-stream
                      :stream *stream*)))
  (handler-bind
      ((yacc-parse-error
        #'(lambda (error)
            (error 'yacc-parse-error-with-position
                   :terminal (yacc-parse-error-terminal error)
                   :value (yacc-parse-error-value error)
                   :expected-terminals (yacc-parse-error-expected-terminals
                                        error)
                   :stream-line-number (line-count-of counting-stream)
                   :stream-column-number (col-count-of counting-stream)))))
    (parse-with-lexer *lexer* *parser*)))

