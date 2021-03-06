(define (make-semaphore n)
  (let ((mutex (make-mutex))
		(counter n))
	(define (the-semaphore m)
	  (cond ((eq? m 'acquire)
			 (mutex 'acauire)
			 (if (> counter 0)
				 (begin (set! counter (- counter 1))
						(mutex 'release))
				 (begin (mutex 'release)
						(the-semaphore 'acquire)))) ;; attention here
			((eq? m 'release)
			 (mutex 'acquire)
			 (if (< counter n)
				 (set! counter (+ counter 1)))
			 (mutex 'release))
			(else (error "unknown request--" m))))
	the-semaphore))
			 

(define (make-semaphore2 n)
  (let ((counter n)
		(cell (list false)))

	(define (acquire-cell)
	  (if (test-and-set! cell)
		  (acquire-cell)))

	(define (the-semaphore m)
	  (cond ((eq? m 'acquire)
			 (acquire-cell)
			 (if (> counter 0)
				 (begin (set! counter (- counter 1))
						(clear! cell))
				 (begin (clear! cell)
						(the-semaphore 'acquire))))
			((eq? m 'release)
			 (acquire-cell)
			 (if (< counter n)
				 (set! counter (+ counter 1)))
			 (clear! cell))
			(else (error "unknown request--" m))))
	the-semaphore))
				 
				 