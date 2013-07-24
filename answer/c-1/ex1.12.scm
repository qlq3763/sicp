(define (pascal-trig row col)
  (cond ((or (<= col 1) (>= col row)) ;; may refinement here
		 1)
		(else (+ (pascal-trig (- row 1) (- col 1))
				 (pascal-trig (- row 1) col)))))
	   
(assert '(= (pascal-trig 1 1) 1))
(assert '(= (pascal-trig 2 1) 1))
(assert '(= (pascal-trig 2 2) 1))

(assert '(= (pascal-trig 3 2) 2))
(assert '(= (pascal-trig 4 2) 3))

(assert '(= (pascal-trig 5 1) 1))
(assert '(= (pascal-trig 5 2) 4))
(assert '(= (pascal-trig 5 3) 6))
(assert '(= (pascal-trig 5 4) 4))
(assert '(= (pascal-trig 5 5) 1))