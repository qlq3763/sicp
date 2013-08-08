(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment p-s p-e)
  (cons p-s p-e))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (let ((p-s (start-segment s))
		(p-e (end-segment s)))
    (make-point (average (x-point p-s)
						 (x-point p-e))
				(average (y-point p-s)
						 (y-point p-e)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define p-s (make-point 2 4))
(define p-e (make-point 4 2))
(define s (make-segment p-s p-e))

(print-point p-s)
(print-point p-e)
(print-point (midpoint-segment s))