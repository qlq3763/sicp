;; form of eath record: (key-list value left-tree-ptr right-tree-ptr)
(define (get-key-list record-ptr) (car record-ptr))
(define (get-val record-ptr) (cadr record-ptr))
(define (get-left-ptr record-ptr) (caddr record-ptr))
(define (get-right-ptr record-ptr) (cdddr record-ptr))

;; (define (set-key-list! record-ptr key-list) 
;;   (set-car! record-ptr key-list))
(define (set-val! record-ptr val) 
  (set-car! (cdr record-ptr) val))
(define (set-left-ptr! record-ptr left-ptr)
  (set-car! (cddr record-ptr) left-ptr))
(define (set-right-ptr! record-ptr right-ptr)
  (set-cdr! (cddr record-ptr) right-ptr))

(define (make-table less-than? same?)
  ;; empty key-list get false
  (define (assoc key-list root-ptr)
	(define (iter key-list root-ptr)
	  (cond ((null? root-ptr)
			 false)
			((same? key-list (get-key-list root-ptr))
			 root-ptr) ;; in case value itself is '() or false
			((less-than? key-list (get-key-list root-ptr))
			 (iter key-list (get-left-ptr root-ptr)))
			(else
			 (iter key-list (get-right-ptr root-ptr)))))
	(if (null? key-list)
		false
		(iter key-list root-ptr)))

  (let ((local-table '()))
    (define (lookup key-list)
      (let ((record (assoc key-list local-table)))
		(if record
			(get-val record)
			false)))

    (define (insert! key-list value)
	  (let ((new-record (cons key-list (cons value (cons '() '())))))
		(define (add-record parent cur)
		  (cond ((null? cur) ;; can not be equal here
				 (if (less-than? key-list (get-key-list parent))
					 (set-left-ptr! parent new-record)
					 (set-right-ptr! parent new-record)))
				((same? key-list (get-key-list cur))
				 (set-val! cur value))
				((less-than? key-list (get-key-list cur))
				 (add-record cur (get-left-ptr cur)))
				(else
				 (add-record cur (get-right-ptr cur)))))

		(if (null? local-table)
			(set! local-table new-record)
			(add-record '() local-table)))
	  (print local-table)
      'ok)    

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (list-same-fn-creator same-key?)
  (define (same-key-list? kl-1 kl-2)
	(cond ((and (null? kl-1) (null? kl-2))
		   true)
		  ((or (not (= (length kl-1) (length kl-2)))
			   (not (same-key? (car kl-1) (car kl-2))))
		   false)
		  (else (same-key-list? (cdr kl-1) (cdr kl-2)))))
  same-key-list?)

(define (list-lessthan-fn-creator less?)
  (define (list-less? l1 l2)
	(cond ((and (null? l1) (null? l2))
		   false) ;; equal case
		  ((or (and (not (null? l2)) 
					(null? l1))
			   (and (not (null? l1)) 
					(not (null? l2))
					(less? (car l1) (car l2))))
		   true) ;; less
		  ((or (and (not (null? l1)) 
					(null? l2))
			   (and (not (null? l1)) 
					(not (null? l2))
					(less? (car l2) (car l1))))
		   false) ;; greater
		  (else 
		   (list-less? (cdr l1) (cdr l2)))))
  list-less?)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
(define lessthan-fn (list-lessthan-fn-creator <))
(define same-fn (list-same-fn-creator =))
(define operation-table (make-table lessthan-fn same-fn))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;;;;;;;;;;;;;;
(print (put '(1 2) '12))
(print (put '(1) '1))
(print (put '(2) '2))

(define tr1 '((1 2) 12 ((1) 1 ()) (2) 2 ()))
(print (get-val tr1))
(print (get-left-ptr tr1))
(print (get-right-ptr tr1))


(print (put '(3) '3))
(print (put '(5) '5))
(print (put '(0 100) '0100))
(print (put '(1 1) '11))
(print (get '(1 2)))
(print (get '(1)))
(print (get '(2)))
(print (get '(1 2 3)))
(print (get '()))