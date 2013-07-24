;; display the content following by a new line
(define (print content)
  (display content)
  (display "\n")
  true) ;; just don't like "unspecified return value"

(define (assert exp)
  (if (not (eval exp user-initial-environment))
	  (begin (print "\n assertion failure:")
			 (print exp)
			 (abort))
	  true))

(define (float=? f1 f2)
  (< (abs (- f1 f2)) 0.00001))
