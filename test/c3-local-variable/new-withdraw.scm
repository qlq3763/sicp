(define new-withdraw
  (let ((balance 100))
	(lambda (amount)
	  (if (>= balance amount)
		  (begin (set! balance (- balance amount))
				 balance)
		  "Insufficient funds"))))

;; I actually couldn't understand these, why it didn't initilize
;; balance every call to new-withdraw, it seems that the let part
;; just get executed for one time.
;; Why????
(display (new-withdraw 10))
(display (new-withdraw 10))
(display (new-withdraw 10))
(display (new-withdraw 10))
;; (new-withdraw 10)
;; (new-withdraw 10)
;; (new-withdraw 10)