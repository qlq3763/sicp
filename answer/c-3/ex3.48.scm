(define (make-account balance)

  (define (make-numbered-account number balance)
	(define (withdraw amount)
	  (if (>= balance amount)
		  (begin (set! balance (- balance amount))
				 balance)
		  "Insufficient funds"))

	(define (deposit amount)
	  (set! balance (+ balance amount))
	  balance)

	(let ((protected (make-serializer)))
	  (define (dispatch m)
		(cond ((eq? m 'withdraw) (protected withdraw))
			  ((eq? m 'deposit) (protected deposit))
			  ((eq? m 'balance) balance)
			  ((eq? m 'serializer) protected)
			  ((eq? m 'number) number)
			  (else (error "unknown request -- MAKE-ACCOUNT" m))))
	  dispatch))

  (let ((base -1)
		(mutex (make-mutex)))
	(mutex 'acquire)
	(set! base (+ base 1))
	(let ((new-account (make-numbered-account base balance)))
	  (mutex 'release)
	  new-account)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
		(serializer2 (account2 'serializer))
		(num1 (account1 'number))
		(num2 (account2 'number)))
	(if (> num1 num2)
		((serializer1 (serializer2 exchange))
		 account1
		 account2)
		((serializer2 (serializer1 exchange))
		 account1
		 account2))))

;; I think this version will try to lock the higher-numbered account
;; first, although most answers from the web use this version. Here, by
;; lock I mean mutex. However, the textbook says:"always attempt to enter
;; procedure protecting the lowest-numbered account first", if this means
;; calling serializer first, then this version is correct.

;; Whatever the order is, this method will prevent deadlock provided that
;; the order is exercised consistently. 