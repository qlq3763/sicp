; My understanding of the form of a solution: (x x x x x x x x), 
; where x stands for a number between 1 and 8, and each x means that
; there is a queen x row in the corresponging column. For example, the 
; solution given by figure 2.8 represented in this way will be:
; (3 7 2 8 5 1 4 6). Representing a solution in this way, the procedure will
; not need the parameter k, which means a column, because the information of 
; column is implicated by the position of a row number in the list.

; Alternatily, a solution can be represented as 
; ((c-x r-x) (c-x r-x) (c-x r-x) (c-x r-x) (c-x r-x) (c-x r-x) (c-x r-x) 
; (c-x r-x)), where a c-x represent a column number and between 1 and 8
; and r-x a row number. 

; Now consider the implementation of safe? The second form of representation 
; of solutions has all the information need to implement safe? And the first
; form of representation also has these information, but the column number is
; implicated instead of stored directly.

; Okay, I will implement both of these represenations and 
; feel the differences. 

; first form
(define (queens-1 board-size)
  (define empty-board '())

  (define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list new-row)))

  (define (safe? k positions)
    (define (test new-col new-row cur-col positions)
      (cond ((null? positions)
	     true)
	    (else
	     (let ((cur-row (car positions)))
	       (if (or (= new-row cur-row)
		       (= (abs (- new-row cur-row)) ;forgot abs first time
			  (abs(- new-col cur-col))))
		   false
		   (test new-col new-row (+ cur-col 1) (cdr positions)))))))

    (if (null? positions) ; redudant
	true                             ; if to protest this
	(test k 
	      (list-ref positions (- k 1))
	      1                                ; ugly mistake
	      (list-head positions (- k 1))))) ; use cdr positions first time

  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))

;; second form
(define (queens-2 board-size)
  (define empty-board '())

  (define (adjoin-position new-row k rest-of-queens)
    (cons (list k new-row) rest-of-queens))

  (define (first-position positions)
    (car positions))

  (define (get-col p)
    (car p))
  
  (define (get-row p)
    (cadr p))

  (define (safe? k positions)
    (define (test new-col new-row rest-positions)
      (cond ((null? rest-positions)
    	     true)
    	    (else
    	     (let ((cur-col (get-col (first-position rest-positions)))
    		   (cur-row (get-row (first-position rest-positions))))
    	       (if (or (= new-row cur-row)
    		       (= (abs (- new-row cur-row)) ;forgot abs first time
    			  (abs(- new-col cur-col))))
    		   false
    		   (test new-col new-row (cdr rest-positions)))))))
	  
    (if (null? positions) ; redudant
    	true                             ; if to protest this
    	(test k
    	      (get-row (first-position positions))
    	      (cdr positions))))

  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))

;; third form: reverse order of first form
(define (queens-3 board-size)
  (define empty-board '())

  (define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))

  (define (safe? k positions)
    (define (test new-col new-row cur-col positions)
      (cond ((null? positions)
	     true)
	    (else
	     (let ((cur-row (car positions)))
	       (if (or (= new-row cur-row)
		       (= (abs (- new-row cur-row)) ;forgot abs first time
			  (abs(- new-col cur-col))))
		   false
		   (test new-col new-row (- cur-col 1) (cdr positions)))))))

    (if (null? positions) ; redudant
	true                             ; if to protest car cdr
	(test k 
	      (car positions)
	      (- k 1)                               
	      (cdr positions)))) 

  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (queen-cols board-size))

;((2 4 1 3) (3 1 4 2))
(print (queens-1 4))
(print (queens-2 4))
(print (queens-3 4))
(print (length (queens-1 8)))
(print (length (queens-2 8)))
(print (length (queens-3 8)))

; note the answer of reversibiity between queens-1 and queens-3

; one thing to note: the relation between adjion-position and safe?
; is tight, very tight in all these three implementations. And this
; relation can be decoupled in the second form, but can not in the 
; first and third form.