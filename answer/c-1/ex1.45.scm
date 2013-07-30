(define (nth-root x n)
  (define times (floor (/ (log n) (log 2))))
  (define (x-n-1 y) (/ x (fast-expt y (- n 1))))
  (fixed-point ((repeated average-damp times) x-n-1)
	       1.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (average x y)
  (/ (+ x y) 2))

(print "")
(print (nth-root 4 2))
(print (nth-root 8 3))
(print (nth-root 16 4))
(print (nth-root 32 5))
(print (nth-root 64 6))
(print (nth-root 128 7))