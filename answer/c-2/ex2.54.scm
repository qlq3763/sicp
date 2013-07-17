; be able to handle (nested)list of symbol and number
;; (define (my-equal? a b)
;;   (cond ((not (= (length a) (length b))) ; 0
;; 	 false)
;; 	((and (null? a) (null? b)) ;1
;; 	 true)
;; 	((and (pair? (car a)) (pair? (car b))) ;2
;; 	 (and (my-equal? (car a) (car b))
;; 	      (my-equal? (cdr a) (cdr b))))
;; 	((or (and (number? (car a)) ;3
;; 		  (number? (car b)) 
;; 		  (= (car a) (car b)))
;; 	     (eq? (car a) (car b)))
;; 	 (my-equal? (cdr a) (cdr b)))
;; 	(else false))) ;4

(define (my-equal? a b)
  (cond ((and (null? a) (null? b)) ; 1
	 true)
	((and (pair? a) (pair? b)) ; 2
	 (and (my-equal? (car a) (car b))
	      (my-equal? (cdr a) (cdr b))))
	((and (not (pair? a)) (not (pair? b))) ;3
	 (eq? a b))
	(else false))) ; 4

(define l-1 (list 1 2 3))
(define l-2 (list 2 (list 2) 3))
(define l-3 (list 'a 'b 'c))

(print (my-equal? nil nil)) ; true
(print (my-equal? l-1 l-1)) ; true
(print (my-equal? l-2 l-2)) ; true
(print (my-equal? l-1 l-2)) ; false
(print (my-equal? l-1 nil)) ; false

(print (my-equal? l-3 l-3)) ; true