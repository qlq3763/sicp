(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner 
			    null-value 
			    term 
			    (next a)
			    next
			    b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a)
	      (combiner (term a) result))))
  (iter a null-value))
		    
(assert '(= (sum identity 1 inc 0) 0))
(assert '(= (sum identity 1 inc 1) 1))
(assert '(= (sum identity 1 inc 4) 10))

(assert '(= (product identity 1 inc 0) 1))
(assert '(= (product identity 1 inc 1) 1))
(assert '(= (product identity 1 inc 4) 24))

(assert '(= (accumulate-iter * 1 identity 1 inc 0) 1))
(assert '(= (accumulate-iter * 1 identity 1 inc 1) 1))
(assert '(= (accumulate-iter * 1 identity 1 inc 4) 24))

