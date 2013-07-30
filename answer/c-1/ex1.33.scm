(define (filtered-accumulate filter combiner null-value
			     term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((if (filter a)
	       (iter (next a) (combiner (term a) result))
	       (iter (next a) result)))))
  (iter a null-value))

(define (sum-square-prime a b)
  (filtered-accumulate prime? + 0 
		       square a inc b))

(define (product-relative-prime n)
  (define (relative-prime? a)
    (= (gcd n a) 1))
  (filtered-accumulate relative-prime? * 1
		       identity 1 inc n))
		       

(assert '(= (sum-square-prime 1 5) 38)) ;; 2 3 5
(assert '(= (product-relative-prime 10) 189)) ;; 1 3 7 9
