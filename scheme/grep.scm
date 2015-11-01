
(define (main args) 
   (cond 
      ((null? (cdr args)) 
       (fprintf (current-error-port) "Usage: grep STRING [FILE]...") 
       (exit 0)) 
      (else 
       (let ((t (kmp-table (cadr args)))) 
	  (for-each (lambda (f) (grep-file t f)) (cddr args)))))) 

(define (grep-file t file) 
   (let* ((mm (open-mmap file read: #t write: #f)) 
	  (ls (mmap-length mm))) 
      (let loop ((o 0)) 
	 (unless (>=fx o ls) 
	    (let ((n (kmp-mmap t mm o))) 
	       (when (>fx n 0) 
		  (print file ":" (mmap-line mm ls n)) 
		  (loop (+fx n 1)))))) 
      (close-mmap mm))) 

(define (mmap-line mm ls n) 
   (let ((b 0) 
	 (e (elong->fixnum ls))) 
      ;; beginning 
      (let loop ((i n)) 
	 (when (>fx i 0) 
	    (if (char=? (mmap-ref mm i) #\Newline) 
		(set! b (+fx i 1)) 
		(loop (-fx i 1))))) 
      ;; end 
      (let loop ((i n)) 
	 (when (<fx i ls) 
	    (if (char=? (mmap-ref mm i) #\Newline) 
		(set! e i) 
		(loop (+fx i 1))))) 
      (mmap-substring mm b (- e b))))
