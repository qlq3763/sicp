(define (iterative-improve good-enough? improve)
  (define (iter guess)
      (if (good-enough? guess)
	  guess
	  (iter (improve guess))))
  iter)

(define (sqrt x)
  (define tolerance 0.0001)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- guess (improve guess))) tolerance))
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f first-guess)
  (define tolerance 0.000001)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve good-enough? f) first-guess))

(define (cubic-root x)
  (fixed-point (lambda (y) (average y (/ x (square y))))
	       1.0))