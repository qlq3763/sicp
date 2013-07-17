(define (partial-sums s)
  (cons-stream (stream-car s)
	       (stream-map + 
			   (partial-sums s)
			   (stream-cdr s))))

(show-stream (partial-sums integers) 10)
