(define (smallest-divisor-v2 n)
  (define (find-divisor n test-divisor)
    (define (next divisor)
      (if (= divisor 2) 3
	  (+ divisor 2)))
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (next test-divisor)))))

  (define (divides? a b)
    (= (remainder b a) 0))

  (find-divisor n 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))

  (define (divides? a b)
    (= (remainder b a) 0))

  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (timed-call fn param)
  (define (print start-time)
    (display "\n")
    (display param)
    (display ", smallest divisor: ")
    (display (fn param))
    (display " *** :")
    (display (- (runtime) start-time))
    (display "\n"))
  (print (runtime)))

(timed-call smallest-divisor 100000007)
(timed-call smallest-divisor-v2 100000007)
(print "\n\n")

(timed-call smallest-divisor 100000037)
(timed-call smallest-divisor-v2 100000037)
(print "\n\n")

(timed-call smallest-divisor 100000039)
(timed-call smallest-divisor-v2 100000039)
(print "\n\n")

(timed-call smallest-divisor 100000049)
(timed-call smallest-divisor-v2 100000049)
(print "\n\n")
