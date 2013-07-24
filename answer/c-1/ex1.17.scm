(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

(define (fast-mul-rec a b)
  (cond ((= b 0) 0)
	((even? b) (double (fast-mul-rec a (halve b))))
	(else (+ a (fast-mul-rec a (- b 1))))))

;; invariant ab+product
(define (fast-mul-iter a b)
  (define (iter a b product)
    (cond ((= b 0) product)
	  ((even? b) (iter (double a) 
			   (halve b)
			   product))
	  (else (iter a (- b 1) (+ product a)))))
  (iter a b 0))

(define (halve n) (/ n 2))
(define (double n) (* n 2))

(assert '(= (mul 3 4) 12))
(assert '(= (mul 4 0) 0))

(assert '(= (fast-mul-rec 3 4) 12))
(assert '(= (fast-mul-rec 4 0) 0))

(assert '(= (fast-mul-iter 3 4) 12))
(assert '(= (fast-mul-iter 4 0) 0))
