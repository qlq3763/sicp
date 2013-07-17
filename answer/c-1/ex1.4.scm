(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

(print "\n.................")
(print (= (a-plus-abs-b 3 -2) 5))
(print "\n.................")