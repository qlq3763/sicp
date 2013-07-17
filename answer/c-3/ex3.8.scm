(define f
  (let ((state 0))
	(lambda (x)
	  (cond ((= x 0)
			 (set! state 1)
			 0)
			(else
			
			 (if (= state 1)
				 0
				 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; version 2
(define f2
  (let ((state 0)
		(old-state 0))
	(lambda (x)
	  (cond ((= x 0)
			 (set! state 1)
			 0)
			(else
			 (set! old-state state)
			 (set! state 0)
			 (if (= old-state 1)
				 0
				 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; version 3
;; from http://wqzhang.wordpress.com/2009/07/13/sicp-exercise-3-8/
(define f3
  (let ((state 0))
    (define (switch-state x)
      (let ((old-state state))
        (set! state (+ x state))
        old-state))
    switch-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; version 4
(define f4
  (let ((state 0)
		(old-state 0))
	(lambda (x)
	  (let ((rst old-state))
		(set! old-state state)
		(cond ((= x 0)
			   (set! state 0)
			   rst)
			  (else
			   (set! old-state 1)
			   rst))))))

;; Notice the difference between f1, f2, f3, f4
;; What I want is this two "+" expression gives the same result.

;; Actually none of them was what I want. The reason is that the
;; value (f x) returns is highly depend on the previous calls to 
;; f.
(print (+ (f 0) (f 1)))
(print (+ (f 0) (f 1)))

(print (+ (f2 0) (f2 1)))
(print (+ (f2 0) (f2 1)))


(print (+ (f3 0) (f3 1)))
(print (+ (f3 0) (f3 1)))

(print (+ (f4 0) (f4 1)))
(print (+ (f4 0) (f4 1)))
;; (print (f 1))
;; (print (f 1))
;; (print (f 0))
;; (print (f 1))
;; (print (f 0))