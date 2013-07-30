(define (cubic a b c)
  (lambda (x) (+ (* x x x)
				 (* a x x)
				 (* b x)
				 c)))

(define (cubic-zero a b c)
  (newtons-method (cubic a b c) 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print (cubic-zero 3 2 1))
(print ((cubic 3 2 1) (cubic-zero 3 2 1)))