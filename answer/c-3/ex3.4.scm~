(define (make-account balance password)
  (define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds"))

  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)

  ;; (define (fake amount)
  ;; 	"")

  ;; (define (print-error info)
  ;; 	(print info)
  ;; 	fake)

  ;; strange functin, yeah I know
  (define (generate-error-msg amount)
	"Incorrect password") 

  (define (dispatch passwd msg)
	(if (eq? passwd password)
		(cond ((eq? msg 'withdraw) withdraw)
			  ((eq? msg 'deposit) deposit)
			  (else (error "Unknown request -- MAKE-ACCOUNT"
						   msg)))
		generate-error-msg))
  
  dispatch)

(define acc (make-account 100 'pass))

(print ((acc 'pass 'withdraw) 44))
(print ((acc 'pass 'withdraw) 64))
(print ((acc 'pasd 'withdraw) 44))

(print ((acc 'pass 'deposit) 40))
(print ((acc 'pasd 'deposit) 60))