;;Equal to:

(define (make-withdraw initial-amount)
  ( ;; call the outer lambda function
   (lambda (balance)
	 (lambda (amount)
	   (if (>= balance amount)
		   (begin (set! balance (- balance amount))
				  balance)
		   "Insufficient funds")))
   initial-amount)) ;; argument to outer lambda function