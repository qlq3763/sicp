;; (define visited '())
(define (count-pairs x)
  (let ((visited '()))
	(define (count y)
	  (if (or (not (pair? y))
			  (memq y visited))
		  0
		  (begin (set! visited (cons y visited))
				 (+ (count (car y))
					(count (cdr y))
					1))))
	(count x)))

(print (count-pairs n-3))
(print (count-pairs n-4))
(print (count-pairs n-7))
(print (count-pairs n-never))