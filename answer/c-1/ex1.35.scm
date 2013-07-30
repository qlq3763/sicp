(define (fi) 
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

(print "")
(print (fi))