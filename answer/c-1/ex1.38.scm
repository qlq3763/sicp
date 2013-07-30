(define (e k)
  (define (n i) 1)
  (define (d i) 
    (cond ((or (= i 1) (= i 2))
		   i)
		  ((= (remainder i 3) 2)
		   (- i (/ (- i 2) 3)))
		  (else 1)))
  (+ 2.0 (cont-frac n d k)))
	       

(print "")
(print (e 100))