;; the form of each item: (data prev-ptr . next-ptr)

;; helper functions
(define (front-ptr dq) (car dq))
(define (rear-ptr dq) (cdr dq))
(define (set-front-ptr! dq item) (set-car! dq item))
(define (set-rear-ptr! dq item) (set-cdr! dq item))

(define (item-data item-ptr) (car item-ptr))
(define (prev-ptr item-ptr) (cadr item-ptr))
(define (next-ptr item-ptr) (cddr item-ptr))
(define (set-prev-ptr! item-ptr prev-ptr) 
  (set-car! (cdr item-ptr) prev-ptr))
(define (set-next-ptr! item-ptr next-ptr) 
  (set-cdr! (cdr item-ptr) next-ptr))


;; another helper function, to avoid the interpreter print "cycle"

(define (deque-value dq)
  (define (rec ptr)
	(if (null? ptr)
		'()
		(cons (item-data ptr)
			  (rec (next-ptr ptr)))))
  (rec (front-ptr dq)))

;; deque interface
(define (make-deque)
  (cons '() '()))

(define (empty-deque? dq)
  (null? (front-ptr dq)))

(define (front-deque dq)
  (if (empty-deque? dq)
	  (error "FRONT called with an empty deque" dq)
	  (item-data (front-ptr dq))))

(define (rear-deque dq)
  (if (empty-deque? dq)
	  (error "REAR called with an empty deque" dq)
	  (item-data (rear-ptr dq))))

(define (front-insert-deque! dq item)
  (let ((new-pair (cons item (cons '() '()))))
	(cond ((empty-deque? dq)
		   (set-front-ptr! dq new-pair)
		   (set-rear-ptr! dq new-pair)
		   (deque-value dq)
		   )
		  (else
		   (set-next-ptr! new-pair (front-ptr dq))
		   (set-prev-ptr! (front-ptr dq) new-pair)
		   (set-front-ptr! dq new-pair)
		   (deque-value dq)))))

(define (rear-insert-deque! dq item)
  (let ((new-pair (cons item (cons '() '()))))
	(cond ((empty-deque? dq)
		   (set-front-ptr! dq new-pair)
		   (set-rear-ptr! dq new-pair)
		   (deque-value dq))
		  (else
		   (set-prev-ptr! new-pair (rear-ptr dq))
		   (set-next-ptr! (rear-ptr dq) new-pair)
		   (set-rear-ptr! dq new-pair)
		   (deque-value dq)))))

(define (front-delete-deque! dq)
  (if (empty-deque? dq)
	  (error "FRONT-DELETE called with an empty deque" dq)
	  (let ((old-front (front-ptr dq))
			(new-front (next-ptr (front-ptr dq))))
		(set-front-ptr! dq new-front)
		(set-next-ptr! old-front '())
		(if (not (null? new-front))
			(set-prev-ptr! new-front '())
			(set-rear-ptr! dq new-front)) ;; empty
		(deque-value dq))))
		 


(define (rear-delete-deque! dq)
  (if (empty-deque? dq)
	  (error "REAR-DELETE called with an empty deque" dq)
	  (let ((old-rear (rear-ptr dq))
			(new-rear (prev-ptr (rear-ptr dq))))
		(set-rear-ptr! dq new-rear)
		(if (not (null? new-rear))
			(set-next-ptr! new-rear '())
			(set-front-ptr! dq new-rear)) ;; empty
		(set-prev-ptr! old-rear '())
		(deque-value dq))))

(define dq (make-deque))
(print (empty-deque? dq))

(print (front-insert-deque! dq 1))
(print (empty-deque? dq))

(print (rear-insert-deque! dq 2))
(print (empty-deque? dq))

(print (front-insert-deque! dq 3))
(print (empty-deque? dq))

(print (rear-insert-deque! dq 4))
(print (empty-deque? dq))

;;

(print (front-delete-deque! dq))
(print (empty-deque? dq))

(print (rear-delete-deque! dq))
(print (empty-deque? dq))

;; (print (rear-delete-deque! dq))
;; (print (empty-deque? dq))

(print (front-delete-deque! dq))
(print (empty-deque? dq))

(print (rear-delete-deque! dq))
(print (empty-deque? dq))