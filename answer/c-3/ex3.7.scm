;; suppose org-passwd is always correct
;; or suppose make-joint is a previlige-operation

;; (define (make-joint account org-passwd new-passwd)
;;   ;; strange functin, yeah I know
;;   (define (generate-error-msg amount)
;; 	"Incorrect password") 
  
;;   (define (dispatch passwd msg)
;; 	(if (eq? passwd new-passwd)
;; 		(account org-passwd msg) 
;; 		generate-error-msg))

;;   dispatch)

;; (define org-ac (make-account 14 'org))
;; (define new-ac (make-joint org-ac 'org 'new))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; another version, which "checks" the validity of org-passwd
(define (make-joint account org-passwd new-passwd)
  ;; strange functin, yeah I know
  (define (generate-error-msg amount)
	"Incorrect password") 
  
  (define (dispatch passwd msg)
	(if (eq? passwd new-passwd)
		(account org-passwd msg) 
		generate-error-msg))

  ;; didn't think out to add this "layer" the frist time.
  ;; actually, it took a long time for me to get the idea of
  ;; adding this layer
  (let ((rst ((account org-passwd 'withdraw) 0)))
	(if (and (string? rst)
			 (string=? rst "Incorrect password"))
		(print "Incorrect password for original account")
		dispatch)))

(define org-ac (make-account 14 'org))
(define new-ac (make-joint org-ac 'org 'new))
(define new-ac2 (make-joint org-ac 'org 'new2))
(define new-ac3 (make-joint new-ac 'new 'new3))

(define new-err (make-joint org-ac 'og 'err))