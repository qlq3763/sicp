(define (or-gate a1 a2 output)
  (define (or-action-procedure)
	(let ((new-value (logical-or 
					  (get-signal a1)
					  (get-signal a2))))
	  (after-delay or-gate-delay
				   (lambda ()
					 (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (is-signal-valid? s)
  (if (or (= s 0) (= s 1))
	  true
	  false))

(define (logical-or s1 s2)
  (if (and (is-signal-valid? s1)
		   (is-signal-valid? s2))
	  (if (or (= s1 1) (= s2 1))
		  1
		  0)
	  (error "Invalid singal" s1 s2)))

;; test
;; use procedures from digital-circuit-simulator.scm
(define a1 (make-wire))
(define a2 (make-wire))
(define output (make-wire))

(define the-agenda (make-agenda))
(define or-gate-delay 5)

(probe 'a1 a1)
(probe 'a2 a2)
(probe 'output output)

(or-gate a1 a2 output)
