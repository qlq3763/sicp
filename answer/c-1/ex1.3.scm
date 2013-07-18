(define (sum-larger-v1 a b c)
  (define (sum-of-squares a b)
    (+ (square a) (square b)))
  (cond ((and (> a c) (> b c)) (sum-of-squares a b))
		((and (> a b) (> c b)) (sum-of-squares a c))
		(else (sum-of-squares b c))))


(define (sum-larger-v2 a b c)
  (define (smallest-of-three)
    (cond ((and (> a c) (> b c)) c)
		  ((and (> a b) (> c b)) b)
		  (else a)))
  (- (+ (square a) (square b) (square c))
	 (square (smallest-of-three))))

(assert '(= (sum-larger-v1 1 2 3) 13))
(assert '(= (sum-larger-v2 1 2 3) 13))

(assert '(= (sum-larger-v1 1 2 2) 8))
(assert '(= (sum-larger-v2 1 2 2) 8))

