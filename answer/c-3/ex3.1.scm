(define (make-accumulator sum)
  (lambda (addend)
	(begin
	  (set! sum (+ sum addend))
	  sum)))

(define a1 (make-accumulator 4))
(print (a1 4))
(print (a1 -4))

(define a2 (make-accumulator 14))
(print (a2 4))
(print (a2 -4))