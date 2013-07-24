;; use the hint: ab^n as an invariant
(define (fast-expt-iter b n)
  (define (iter b n a)
    (cond ((= n 0) a)
	  ((even? n) (iter (square b) 
			   (/ n 2) 
			   a))
	  (else (iter b 
		      (- n 1)
		      (* a b)))))
  (iter b n 1))

(assert '(= (fast-expt-iter 2 3) 8))
(assert '(= (fast-expt-iter 2 0) 1))
(assert '(= (fast-expt-iter 2 7) 128))
(assert '(= (fast-expt-iter 2 1) 2))
(assert '(= (fast-expt-iter 2 2) 4))
(assert '(= (fast-expt-iter 2 8) 256))
