(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time)))) 

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)) 

(define (search-for-primes a b)
  (define (test-odd-prime s e)
    (cond ((< s e)
	   (timed-prime-test s)
	   (search-for-primes (+ s 2) e))))
  (test-odd-prime (if (odd? a) a
		      (+ a 1))
		  b))
