;; Try to cover the largest interval possible.
(define (sub-interval a b)
  (make-interval (- (lower-bound a)
		    (upper-bound b))
		 (- (upper-bound a)
		    (lower-bound b))))

(define i-2 (make-interval 0 4))

;; the variable i is defined in ex2.7.scm
(display (sub-interval i i-2))