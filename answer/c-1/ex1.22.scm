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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; (search-for-primes 1000 1050)
;; (search-for-primes 10000 10050)
;; (search-for-primes 1000000 1000050)
(search-for-primes 100000000 100000050)
