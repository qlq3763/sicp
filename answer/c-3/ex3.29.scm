;; a or b = not( (not a) and (not b))
(define (or-gate a1 a2 output)
  (let ((not-a1 (make-wire))
		(not-a2 (make-wire))
		(and-of-not (make-wire)))
	(inverter a1 not-a1)
	(inverter a2 not-a2)
	(and-gate not-a1 not-a2 and-of-not)
	(inverter and-of-not output)
	'ok))

(define (is-signal-valid? s)
  (if (or (= s 0) (= s 1))
	  true
	  false))

;; or-gate-delay = and-gate-delay + 2 * inverter-delay.

;; test
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)

(define x (make-wire))
(define y (make-wire))
(define z (make-wire))

(probe 'x x)
(probe 'y y)
(probe 'z z)


(or-gate x y z)
(propagate) ;; execute the action added to agenda by or-gate(some
;; of the action is actually wrong(at wrong time), 
;; although the final result is correct)

;; (probe 'x x)
;; (probe 'y y)
;; (probe 'z z)