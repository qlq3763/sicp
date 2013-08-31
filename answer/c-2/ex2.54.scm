; be able to handle (nested)list of symbol and number
;; assume a and b are both list
;; (define (my-equal? a b)
;;   (cond	((and (null? a) (null? b)) ;1
;; 		 true)
;; 		((not (= (length a) (length b)));2
;; 		 false)
;; 		((and (pair? (car a)) (pair? (car b))) ;3
;; 		 (and (my-equal? (car a) (car b))
;; 			  (my-equal? (cdr a) (cdr b))))
;; 		((and (number? (car a)) (number? (car b)));4
;; 		 (and (= (car a) (car b))
;; 			  (my-equal? (cdr a) (cdr b))))
;; 		((and (symbol? (car a)) (symbol? (car b)));5
;; 		 (and (eq? (car a) (car b))
;; 			  (my-equal? (cdr a) (cdr b))))
;; 		(else false))) ;6

; This version is simpler, and the difference between this two deserves
; attention: the previous version think in terms of (car a) and (car b),
; this version think in terms of a and b.
; Should learn from this. Think in a higher level.
; Another benefit: (my-equal 3 4) works for this "simpler" version, not
; for the above version
(define (my-equal? a b)
  (cond ((and (null? a) (null? b)) ; 1
		 true)
		((and (pair? a) (pair? b)) ; 2
		 (and (my-equal? (car a) (car b))
			  (my-equal? (cdr a) (cdr b))))
		((and (not (pair? a)) (not (pair? b))) ;3
		 (or (and (symbol? a) (symbol? b) (eq? a b))
			 (and (number? a) (number? b) (= a b))))
		(else false))) ; 4

(define l-1 (list 1 2 3))
(define l-2 (list 2 (list 2) 3))
(define l-3 (list 'a 'b 'c))

(assert '(my-equal? nil nil)) 
(assert '(my-equal? l-1 l-1))
(assert '(my-equal? l-2 l-2)) 
(assert '(not (my-equal? l-1 l-2)))
(assert '(not (my-equal? l-1 nil)))
(assert '(my-equal? l-3 l-3))
(assert '(not (my-equal? 3 4)))
(assert '(my-equal? 4 4))
(assert '(not (my-equal? 4.4 4.3)))
(assert '(my-equal? 4.4 4.4))