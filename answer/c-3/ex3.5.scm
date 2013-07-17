;; left lower (x1, y1)
;; right upper (x2, y2)
(define (estimate-integral p x1 x2 y1 y2 trials)
  (define (test)
	(p (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (* (- x2 x1) 
		(- y2 y1))
	 (monte-carlo trials test)))

(define (estimate-pi trials)
  (define (p x y)
	(<= (+ (square x) (square y))
		1.0))
  (estimate-integral p -1.0 1.0 -1.0 1.0 trials))

(define (random-in-range low high)
  (let ((range (- high low)))
	(+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
	(cond ((= trials-remaining 0)
		   (/ trials-passed trials))
		  ((experiment)
		   (iter (- trials-remaining 1) (+ trials-passed 1)))
		  (else
		   (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

;; from http://wqzhang.wordpress.com/2009/07/11/sicp-exercise-3-5/
;; using 2.0 instead of 2 in estimate-integral is primordial. If you pass
;; two integers to (random-in-range low high), it will return another
;; integer strictly inferior to your 'high' value — and this completely
;; screws the Monte-Carlo method (it then estimates pi to ~3.00). 
