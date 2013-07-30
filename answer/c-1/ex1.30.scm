(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (+ (term a) result))))
  (iter a 0))

(define (inc n) (+ n 1))

(assert '(= (sum square 1 inc 0) 0))
(assert '(= (sum square 1 inc 1) 1))
(assert '(= (sum square 1 inc 4) 30))
