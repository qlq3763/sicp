(define (carm? n)
  (and (carm-iter n 2)
       (not (prime? n)))) ;; assure not a "real" prime

(define (carm-iter n base)
  (cond ((= base n)
	 true)
	((= (expmod base n n)
	    base)
	 (carm-iter n (+ base 1)))
	(else false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(assert '(fast-prime? 561 4))
(assert '(fast-prime? 1105 4))
(assert '(fast-prime? 1729 4))
(assert '(fast-prime? 2465 4))
(assert '(fast-prime? 2821 4))
(assert '(fast-prime? 6601 4))

(assert '(not (prime? 561)))
(assert '(not (prime? 1105)))
(assert '(not (prime? 1729)))
(assert '(not (prime? 2465)))
(assert '(not (prime? 2821)))
(assert '(not (prime? 6601)))

(assert '(carm? 561))
(assert '(carm? 1105))
(assert '(carm? 1729))
(assert '(carm? 2465))
(assert '(carm? 2821))
(assert '(carm? 6601))
