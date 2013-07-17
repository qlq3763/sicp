;; a.
;; The personnel file must be type-tagged by its division name. 
;; The type of information must be supplied is: division name.
(define (get-record personnel name)
  ((get 'get-record (division personnel)) (file personnel) name))

(define (division p)
  (type-tag p))

(define (file p)
  (contents p))

;; b.
;; The record be type-tagged by its division name
(define (get-salary record)
  ((get 'get-salary (division record)) (record-content record)))

(define (record-content r)
  (contents r))

;; c.
(define (find-employee-record personnel-list name)
  (if (null? personnel-list)
	  false
	  (let ((result (get-record (car personnel-list) name)))
		(if result
			result
			(find-employee-record (cdr personnel-list) name)))))

;; d.
;; Tag all personnel file and employee record. And "put" the according
;; method for manipulate into the "table".

;; The following is a simple example

(define (install-division-1-pakg)
  (define (get-name record); with division name
	(cadr record))
 
  (define (get-record division name)
	(if (or (null? division) (string<? (symbol->string name) 
									   (symbol->string (get-name (car division)))))
		false
		(if (equal? (get-name (car division)) name)
			(car division)
			(get-record (cdr division) name))))

  (define (get-salary record) ; without division name
	(caddr record))

  (put 'get-record 'division-1 get-record)
  (put 'get-salary 'division-1 get-salary))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-division-2-pakg)
  (define (get-name record) ; with
	(cadddr record))
 
  (define (get-record division name)
	(if (null? division)
		false
		(if (equal? (get-name (car division)) name)
			(car division)
			(get-record (cdr division) name))))

  (define (get-salary record) ; without
	(cadr record))

  (put 'get-record 'division-2 get-record)
  (put 'get-salary 'division-2 get-salary))

;; ordered by name
(define division-1 '(division-1 
					 (division-1 a1 addr-a1 salary-a1) 
					 (division-1 c1 addr-c1 salary-c1) 
					 (division-1 d1 addr-d1 salary-d1)))

(define division-2 '(division-2
					 (division-2 addr-a2 salary-a2 a2) 
					 (division-2 addr-c2 salary-c2 c2)
					 (division-2 addr-d2 salary-d2 d2)))
;; (print division-1)
;; (print division-2)

(install-division-1-pakg)
(install-division-2-pakg)

(print (get-salary (get-record division-1 'a1)))
(print (get-salary (get-record division-1 'c1)))
(print (get-record division-1 'd1))
(print (get-record division-1 'f1))

(print (get-salary (get-record division-2 'a2)))
(print (get-salary (get-record division-2 'c2)))
(print (get-record division-2 'd2))
(print (get-record division-2 'a3))

(print (find-employee-record (list division-1 division-2) 'd1))
(print (find-employee-record (list division-1 division-2) 'c2))
(print (find-employee-record (list division-1 division-2) 'd4))