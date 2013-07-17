(define (make-queue)
  (let ((front-ptr '())
		(rear-ptr '()))

	(define (empty-queue?) (null? front-ptr))

	(define (front-queue)
	  (if (empty-queue?)
		  (error "FRONT called with an empty queue" queue)
		  (car front-ptr)))

	(define (insert-queue! item)
	  (let ((new-pair (cons item '())))
		(cond ((empty-queue?)
			   (set! front-ptr new-pair)
			   (set! rear-ptr new-pair)
			   dispatch)
			  (else
			   (set-cdr! rear-ptr new-pair)
			   (set! rear-ptr new-pair)
			   dispatch)))) 

	(define (delete-queue!)
	  (cond ((empty-queue?)
			 (error "DELETE! called with an empty queue" queue))
			(else
			 (set! front-ptr (cdr front-ptr))
			 dispatch))) 

	(define (print-queue)
	  (print front-ptr))

	(define (dispatch m)
	  (cond ((eq? m 'empty-queue?) (empty-queue?))
			((eq? m 'front-queue) (front-queue))
			((eq? m 'insert-queue!) insert-queue!)
			((eq? m 'delete-queue!) (delete-queue!))
			((eq? m 'print-queue) (print-queue))
			(else
			 (error "Unknown request" m))))

	dispatch))

(define (empty-queue? queue)
  (queue 'empty-queue?))

(define (front-queue queue)
  (queue 'front-queue))

(define (insert-queue! queue item)
  ((queue 'insert-queue!) item))

(define (delete-queue! queue)
  (queue 'delete-queue!))

(define (print-queue queue)
  (queue 'print-queue))

(define q1 (make-queue))
(print (empty-queue? q1))
(print-queue q1)
(print-queue (insert-queue! q1 1))
(print (front-queue q1))
(print (empty-queue? q1))
(print-queue (insert-queue! q1 2))
(print-queue (insert-queue! q1 3))
(print-queue (insert-queue! q1 4))
(print (empty-queue? q1))

;;1
(print-queue (delete-queue! q1))
(print (front-queue q1))

;;2
(print-queue (delete-queue! q1))
(print (front-queue q1))
(print (empty-queue? q1))

;;3
(print-queue (delete-queue! q1))
(print (front-queue q1))
(print (empty-queue? q1))

;; 4
(print-queue (delete-queue! q1))
(print (empty-queue? q1))