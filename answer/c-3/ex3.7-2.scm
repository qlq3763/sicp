(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (display-error amount)
    "Incorrect password")
  (let ((password-list (list password)))
    (define (mkj new-password)
      (set! password-list (cons new-password password-list))
      dispatch)
    (define (dispatch p m)
      (if (memq p password-list)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'make-joint) mkj)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m)))
          display-error))
    dispatch))

(define (make-joint account old-password new-password)
  ((account old-password 'make-joint) new-password))

; test
(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 5)
;Value: 95
(define paul-acc 
  (make-joint peter-acc 'open-sesame 'rosebud))
((paul-acc 'rosebud 'withdraw) 5)
;Value: 90
((peter-acc 'open-sesame 'deposit) 30)
;Value: 120
((peter-acc 'rosebud 'deposit) 30)
;Value: 150
((paul-acc 'open-sesame 'withdraw) 50)
;Value: 100
((paul-acc 'open-sesamexxx 'withdraw) 50)
;Value: "Incorrect password"