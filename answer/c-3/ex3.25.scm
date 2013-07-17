;; what we need is a new same-key-list? method
(define (make-table same-key-list?)
  ;; empty key-list get false
  (define (assoc key-list records)
	(define (iter key-list records)
	  (cond ((null? records) false)
			((same-key-list? key-list (caar records)) (car records))
			(else (assoc key-list (cdr records)))))
	(if (null? key-list)
		false
		(iter key-list records)))

  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (let ((record (assoc key-list (cdr local-table))))
		(if record
			(cdr record)
			false)))

    (define (insert! key-list value)
      (let ((record (assoc key-list (cdr local-table))))
		(if record
			(set-cdr! record value)
			(set-cdr! local-table
					  (cons (cons key-list value) 
							(cdr local-table)))))
      'ok)    

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (make-list-compare same-key?)
  (define (same-key-list? kl-1 kl-2)
	(cond ((and (null? kl-1) (null? kl-2))
		   true)
		  ((or (not (= (length kl-1) (length kl-2)))
			   (not (same-key? (car kl-1) (car kl-2))))
		   false)
		  (else (same-key-list? (cdr kl-1) (cdr kl-2)))))
  same-key-list?)
		
(define operation-table (make-table (make-list-compare equal?)))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;;;;;;;;;;;;;;
(print (put '(1 2) '12))
(print (put '(1) '1))
(print (put '(2) '2))

(print (get '(1 2)))
(print (get '(1)))
(print (get '(2)))
(print (get '(1 2 3)))
(print (get '()))