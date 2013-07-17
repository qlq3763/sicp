(define (make-monitored f)
  (let ((count 0))
	(lambda (m)
	  (cond ((eq? m 'how-many-calls?) 
			 count)
			((eq? m 'reset-count) 
			 (set! count 0)
			 count)
			(else 
			 (set! count (+ count 1))
			 (f m))))))

(define m-sqrt (make-monitored sqrt))

(print (m-sqrt 100))
(print (m-sqrt 'how-many-calls?))

(print (m-sqrt 9))
(print (m-sqrt 'how-many-calls?))

(m-sqrt 'reset-count)

(print (m-sqrt 'how-many-calls?))
(print (m-sqrt 16))
(print (m-sqrt 'how-many-calls?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define m-sqrt2 (make-monitored sqrt))

(print (m-sqrt2 100))
(print (m-sqrt2 'how-many-calls?))

(print (m-sqrt2 9))
(print (m-sqrt2 'how-many-calls?))

(m-sqrt2 'reset-count)

(print (m-sqrt2 'how-many-calls?))
(print (m-sqrt2 16))
(print (m-sqrt2 'how-many-calls?))
