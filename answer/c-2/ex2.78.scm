;;;;;;;;;;;;;;;;;; tag-type operations ;;;;;;;;;;;;;;;;
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number) ;; or (number? contents)
	  contents
	  (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum)
		 (car datum))
		((number? datum)
		 'scheme-number)
		(else error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((pair? datum)
		 (cdr datum))
		((number? datum)
		 datum)
		(error "Bad tagged datum -- CONTENTS" datum)))


(define x (make-scheme-number 4))
(define y (make-scheme-number 14))

(newline)
(print x)
(print y)
(print (add x y))
(print (div x y))

(print (add 4 14))
(print (div 4 14))