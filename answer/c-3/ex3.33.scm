(define (averager a b c)
  (let ((p (make-connector))
		(h (make-connector)))
	(adder a b p)
	(multiplier p h c)
	(constant 1/2 h)))

;; test
(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(probe 'a a)
(probe 'b b)
(probe 'c c)

(averager a b c)