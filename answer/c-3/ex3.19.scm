(define (cycle-2? l)
  (define (check slow fast)
	(cond ((or (not (pair? slow))
			   (not (pair? fast))
			   (not (pair? (cdr fast))))
		   false)
		  ((eq? slow fast)
		   true)
		  (else
		   (check (cdr slow) (cddr fast)))))
  (if (not (pair? l))
	  false
	  (check l (cdr l))))


(print (cycle-2? '()))
(print (cycle-2? '(1)))
(print (cycle-2? '(1 2 3)))

(print (cycle-2? (make-cycle '(1))))
(print (cycle-2? (make-cycle '(1 2))))
(print (cycle-2? (make-cycle '(1 2 3))))
(print (cycle-2? (make-cycle '(1 2 3 4))))