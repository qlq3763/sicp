(define (double f)
  (lambda (x) (f (f x))))

(define (four f)
  (lambda (x) (f (f (f (f x))))))

(define (inc n) (+ n 1))

(print "")
(print (((double (double double)) inc) 5))