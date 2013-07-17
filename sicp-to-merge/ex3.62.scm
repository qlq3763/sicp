;; s1 / s2
(define (div-series s1 s2)
  (mul-series s1
	      (if (= 1 (stream-car s2))
		  (invert-unit-series s2)
		  (scale-stream (invert-unit-series (scale-stream s2 (/ 1 (stream-car s2))))
				(/ 1 (stream-car s2))))))

(define tan-series (div-series sine-series cosine-series))
(show-stream tan-series 14)

;; See this web page for power series for tangent:
;; http://zh.wikipedia.org/wiki/%E6%B3%B0%E5%8B%92%E5%B1%95%E9%96%8B%E5%BC%8F
