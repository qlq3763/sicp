(define (repeated f n)
  (if (= n 1) 
      f
      (compose f (repeated f (- n 1)))))

(print "")
(print ((repeated square 3) 2))