(define (my-cons x y)
  (lambda (m) (m x y)))

(define (my-car z)
  (z (lambda (p q) p)))

(define (my-cdr z)
  (z (lambda (p q) q)))

(define z (my-cons 4 14))

(newline)
(display (my-car z))

(newline)
(display (my-cdr z))