(define (compose f g)
  (lambda (x) (f (g x))))

(print "")
(print ((compose square inc) 3))