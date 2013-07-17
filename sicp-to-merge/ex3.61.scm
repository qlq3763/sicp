(define (invert-unit-series s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (invert-unit-series s))
			       -1)))

(show-stream (invert-unit-series cosine-series) 14)

;; See this web page for the power series for sec:
;; http://zh.wikipedia.org/wiki/%E6%B3%B0%E5%8B%92%E5%B1%95%E9%96%8B%E5%BC%8F
