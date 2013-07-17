(define dx 1)

(define (smooth f)
  (define (average a b c)
    (/ (+ a b c) 3))
  (lambda (x) (average (f (- x dx))
		       (f x)
		       (f (+ x dx)))))

(define (smooth-n f n)
  ((repeated smooth n) f))