(define (for-each proc items)
  (cond ((not (null? items)) ;; two statements here, so cond used, not if
	 (proc (car items))
	 (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))
	  (list 57 321 88))
      