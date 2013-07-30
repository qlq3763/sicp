(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      ;; (display next)
      ;; (newline)
	  (print next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-x-1000)
  (fixed-point (lambda (x) (/ (log 1000)
							  (log x)))
			   2.0))

(define (x-x-1000-v2)
  (fixed-point (lambda (x) (average x
									(/ (log 1000) (log x))))
			   2.0))

(print "")
(print (x-x-1000))
(print "*****************\n\n")
(print (x-x-1000-v2))


