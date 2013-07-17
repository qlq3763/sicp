;; determin whether a list(flat list) contains a cycle

(define (cycle? l)
  (let ((visited '()))
	(define (check x)
	  (cond ((not (pair? x))
			 false)
			((memq x visited)
			 true)
			(else
			 (set! visited (cons x visited))
			 (check (cdr x)))))
	(check l)))

(print (cycle? '()))
(print (cycle? '(1)))
(print (cycle? '(1 2 3)))

(print (cycle? (make-cycle '(1))))
(print (cycle? (make-cycle '(1 2 3))))