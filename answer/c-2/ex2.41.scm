  

(define (sum-s-triples n s)
  (define (unique-triples n) ;; 1 <= k < j < i <= n
    (flatmap (lambda (i)
	       (map (lambda (pair) (cons i pair))
		    (unique-pairs (- i 1))))
	     (enumerate-interval 1 n)))

  (define (sum-triple triple)
    (accumulate + 0 triple))

  (define (sum-s? triple)
    (= s (sum-triple triple)))

  (define (make-triple-sum triple)
    (list (car triple)
	  (cadr triple)
	  (caddr triple)
	  (sum-triple triple)))

  (map make-triple-sum 
       (filter sum-s?
	       (unique-triples n))))

(print (sum-s-triples 6 7))