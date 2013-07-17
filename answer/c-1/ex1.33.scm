(define (filtered-accumulate filter combiner null-value
			     term a next b)
  (define (iter a result)
    (cond ((> a b) result)
	  ((if (filter a)
	       (iter (next a) (combiner (term a) result))
	       (iter (next a) result)))))
  (iter a null-value))

(define (next n) (+ n 1))

(define (sum-square-prime a b)
  (filtered-accumulate prime? + 0 
		       square a next b))

(define (product-relative-prime n)
  (define (term a) a)
  (define (relative-prime? a)
    (= (gcd n a) 1))
  (filtered-accumulate relative-prime? * 1
		       term 1 next n))
		       
;; prime?

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; gcd
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
