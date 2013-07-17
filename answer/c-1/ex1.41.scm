(define (double f)
  (lambda (x) (f (f x))))

(define (four f)
  (lambda (x) (f (f (f (f x))))))

(define (inc n) (+ n 1))
