(define (make-account balance password)
  (define (withdraw amount)
	(if (>= balance amount)
		(begin (set! balance (- balance amount))
			   balance)
		"Insufficient funds"))

  (define (deposit amount)
	(set! balance (+ balance amount))
	balance)

  ;; strange functin, yeah I know
  (define (generate-error-msg amount)
	"Incorrect password") 

  (define (call-the-cops)
	(print "has called the cops"))

  ;; need to call ((acc2 'passd 'deposit) 4) explicitly!!!!
  ;; to update tries
  ;; (let ((tries 0))
  ;; 	(lambda (passwd msg)
  ;; 	  (if (eq? passwd password)
  ;; 		  (begin (set! tries 0)
  ;; 				 (cond ((eq? msg 'withdraw) withdraw)
  ;; 					   ((eq? msg 'deposit) deposit)
  ;; 					   (else (error "Unknown request -- MAKE-ACCOUNT"
  ;; 									msg))))
  ;; 		  (begin 
  ;; 			(set! tries (+ tries 1))
  ;; 			(if (> tries 7) (call-the-cops))
  ;; 			generate-error-msg)))))

  (let ((tries 0))
  	(define (dispatch passwd msg)
	  (if (eq? passwd password)
		  (begin (set! tries 0)
				 (cond ((eq? msg 'withdraw) withdraw)
					   ((eq? msg 'deposit) deposit)
					   (else (error "Unknown request -- MAKE-ACCOUNT"
									msg))))
		  (begin 
			(set! tries (+ tries 1))
			(if (> tries 7) (call-the-cops)) ;; if ends here
			generate-error-msg)))
	dispatch))
;; (define acc (make-account 100 'pass))

;; (print ((acc 'pass 'withdraw) 44))
;; (print ((acc 'pass 'withdraw) 64))
;; (print ((acc 'pasd 'withdraw) 44))

;; (print ((acc 'pass 'deposit) 40))
;; (print ((acc 'pasd 'deposit) 60))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define acc2 (make-account 100 'pass))

;; call function n times
(define (n-times n f arg)
  (if (> n 0)
	  (begin (f arg)
			 (n-times (- n 1) f arg))))

;; these are wrong, incorrect will be euqal to generate-error-msg and
;; correct will be equal to deposit, won't need to get the lambda clouse
;; again, tries will not get updated

;; (define correct (acc2 'pass 'deposit))
;; (define incorrect (acc2 'pasd 'deposit))

;; need to call ((acc2 'passd 'deposit) 4) explicitly!!!!

;; Please Note the difference between the following three:

;; 1#
;; ((acc2 'pasd 'deposit) 4)
;; ((acc2 'pasd 'deposit) 4)
;; ((acc2 'pasd 'deposit) 4)
;; ((acc2 'pasd 'deposit) 4)

;; 2#
;; (incorrect 4)
;; (incorrect 4)
;; (incorrect 4)
;; (incorrect 4)

;; 3# 
;; (n-times 4 incorrect 4)

;; Case 2# and 3# have the same effect, they just update "tries" just once, 
;; when incorrect was defined. However, Case 1# update "tries" four times, 
;; which is what we actually want.