(define (pascal-trig row col)
  (cond ((or (<= col 1) (>= col row)) ;; may refinement here
	 1)
	(else (+ (pascal-trig (- row 1) (- col 1))
		 (pascal-trig (- row 1) col)))))
	   