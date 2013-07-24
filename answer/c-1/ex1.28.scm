(define (expmod-2 base exp m)
  (define (nontrival? a)
    (and (not (= a (- m 1)))
	 (not (= a 1))
	 (= (remainder (square a) m) 1)))
  (define (test-val a)
    (if (nontrival? a) 0
	(remainder (square a) m))) ;;redundant
  (cond ((= exp 0) 1)
        ((even? exp)
         (test-val (expmod-2 base (/ exp 2) m)))
        (else
	 (remainder (* base (expmod-2 base (- exp 1) m))
                    m))))  

(define (miller-rabin-test n)
  (define (try-it a)
    (cond ((= (expmod-2 a (- n 1) n) 1) true)
	  (else false)))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime-2? n times)
  (cond ((= times 0) true)
	((miller-rabin-test n) (fast-prime-2? n (- times 1)))
	(else false)))

(assert '(fast-prime-2? 2 4))
(assert '(fast-prime-2? 3 4))
(assert '(fast-prime-2? 19 4))
(assert '(not (fast-prime-2? 21 4)))
(assert '(not (fast-prime-2? 27 4)))

;; Carmichael numbers
(assert '(not (fast-prime-2? 561 4)))
(assert '(not (fast-prime-2? 1105 4)))
(assert '(not (fast-prime-2? 1729 4)))
(assert '(not (fast-prime-2? 2465 4)))
(assert '(not (fast-prime-2? 2821 4)))
(assert '(not (fast-prime-2? 6601 4)))
