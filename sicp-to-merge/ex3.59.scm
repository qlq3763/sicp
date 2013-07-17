(define (integrate-series s)
  (stream-map / s (integers-starting-from 1)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series)
			       -1)))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(show-stream (integrate-series ones) 14)
(show-stream exp-series 14)
(show-stream cosine-series 14)
(show-stream sine-series 14)
