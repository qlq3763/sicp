(define (cube-root x)
  (define (good-enough? guess next)
	(define frac 0.001)
	(< (abs (- guess next)) (* frac guess)))

  (define (improve guess x)
	(/ (+ (/ x (square guess))
		  (* 2 guess))
	   3))

  (define (cube-root-iter guess x)
    (let ((next (improve guess x)))
      (if (good-enough? guess next)
		  next
		  (cube-root-iter next x))))
  (cube-root-iter 1.0 x))

(print "\n.............")

(print (cube-root 8))
(print (cube-root 27))

(print "\n.............")