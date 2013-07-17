(define (new-sqrt x)
  (define (sqrt-iter guess x)
	(let ((next (improve guess x)))
	  (if (good-enough? guess next)
		  next
		  (sqrt-iter next x))))

  (define (improve guess x)
	(average guess (/ x guess)))

  (define (average x y)
	(/ (+ x y) 2))

  (define (good-enough? guess next)
	(define frac 0.001)
	(< (abs (- guess next)) (* frac guess)))
  (sqrt-iter 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (original-sqrt x)
  (define (sqrt-iter guess x)
	(if (good-enough? guess x)
		guess
		(sqrt-iter (improve guess x)
				   x)))

  (define (improve guess x)
	(average guess (/ x guess)))

  (define (average x y)
	(/ (+ x y) 2))

  (define (good-enough? guess x)
	(< (abs (- (square guess) x)) 0.001))
  (sqrt-iter 1.0 x))

;; small numbers
(print "\n.................")

(define n1 0.000001)
(print (original-sqrt n1))
(print (new-sqrt n1))

(print "\n.................")