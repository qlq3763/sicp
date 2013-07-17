;; fake fake "random" test
(define random-init 1)
(define (rand-update x)
  (+ x 1))

(define rand
  (let ((x random-init))
	(define (reset new-val)
	  (set! x new-val)
	  x)

	(define (dispatch msg)
	  (cond ((eq? msg 'generate)
			 (set! x (rand-update x))
			 x)
			((eq? msg 'reset)
			 reset)
			(else
			 (error "Unknown requent -- RAND" msg))))
	dispatch))




;; Strange
;; (define x 14)

;; (define (reset1 new-val)
;;   (set! x new-val)
;;   new-val)

;; (define (reset2 new-val)
;;   (set! x new-val)
;;   x)

;; This two has different value, reset1 has new-val, but reset2 has
;; old x

;; Now could not get that strange behavior, ...