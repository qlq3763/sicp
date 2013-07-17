(define (ripple-carry-adder A B S C)
  (define (iter A B S C)
	(let ((c-in (make-wire)))
	  (if (null? (cdr A))
		  (set-signal! c-in 0)
		  (iter (cdr A) (cdr B) (cdr S) c-in))
	  (full-adder (car A) (car B) c-in (car S) C)))
  (if (and (> (length A) 0) 
		   (= (length A) (length B))
		   (= (length B) (length S)))
	  (iter A B S C)
	  (error "invald binary number" A B S)))

;; test
;; use procedures from digital-circuit-simulator.scm
(define (make-wire-list n)
  (if (= n 0)
	  '()
	  (cons (make-wire) (make-wire-list (- n 1)))))

(define sig-0 (make-wire))
(define sig-1 (make-wire))
(set-signal! sig-1 1)

(define A (list sig-1 sig-1 sig-0))
(define B (list sig-0 sig-1 sig-1))
(define S (make-wire-list 3))
(define C (make-wire))

(ripple-carry-adder A B S C)
(propagate) ;; important!!!!

(print (map get-signal S))
(print (get-signal C))

