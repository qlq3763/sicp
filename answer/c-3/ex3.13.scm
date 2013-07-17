(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

(define (last-pair x)
  (if (null? (cdr x))
	  x
	  (begin (print (car x))
			 (last-pair (cdr x)))))