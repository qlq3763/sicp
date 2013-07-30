(define (tan-cf x k)
  (define (double-minus-one x)
    (- (* 2 x) 1))
  (define (iter i result)
    (cond ((= i 0) result)
		  ((= i 1) 
		   (iter (- i 1) 
				 (/ x (- 1 result))))
		  (else (iter (- i 1)
					  (/ (* x x) 
						 (- (double-minus-one i) result)))))) 
  (iter k 0))

;; get this one on the second try
(define (tan-cf-v2 x k)
  (define (n i)
	(if (= i 1)
		x
		(- (square x))))

  (define (d i) (- (* 2 i) 1))

  (cont-frac n d k))

(print "")
(define pi 3.14159265 )
(print (tan-cf (/ pi 4) 100))
(print (tan-cf-v2 (/ pi 4) 100))