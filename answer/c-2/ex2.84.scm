(define (apply-generic op . args)

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
		  (if (not (check-type-list type-tags))
			  (error "No method for these types"
					 (list op type-tags))
			  (apply apply-generic
					 (append (list op)
							 (raise-to-highest args
											   (find-highest-type type-tags)))))))))

;;;; hierarchy of types, low -> high
(define hierarchy '(scheme-number rational complex))


;;;;;;; chcek 
(define (check-type-list type-list)

  (define (same-type? lst)
	(let ((type (car lst)))
	  (every (lambda (x) (eq? x type)) lst)))

  (define (valid-type? type)
	(memq type hierarchy))
  
  (and (not (same-type? type-list))
	   (every valid-type? type-list)))
	  
;;;;;;; find highest type
(define (find-highest-type type-list)
  (define (type-to-position type)
	(- (length hierarchy) (length (memq type hierarchy))))

  (list-ref hierarchy (apply max (map type-to-position type-list))))

;;;;;;;;;;;;;;;; raise
(define (raise-to-highest arg-list target-type)

  (define (raise-to arg target-type)
	(if (eq? (type-tag arg) target-type)
		arg
		(raise-to (raise arg) target-type)))

	(map (lambda (type) (raise-to type target-type)) arg-list))



;;;;;;;;;;;;;;;;;;;; test case ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define s-1 (make-scheme-number 0))
(define s-2 (make-scheme-number 4))

(define r-1 (make-rational 0 4))

(define z-1 (make-complex-from-real-imag 0 0))
(define z-2 (make-complex-from-mag-ang 5 (atan 4 3)))
(define z-3 (make-complex-from-mag-ang 0 (atan 3 3)))

(print (add s-1 s-2))
(print (add s-1 4))
(print (add r-1 r-1))
(print (add z-1 z-3))
(print "same type done\n\n")

(print (add r-1 s-2))
(print (add s-2 r-1))
(print "s-r done\n\n")

(print (add z-1 s-1))
(print (add s-1 z-1))
(print (add z-2 -1))
(print (add -1 z-2))
(print "s-z done\n\n")

(print (add z-2 r-1))
(print (add r-1 z-2))
(print "r-z done\n\n")

;; (define s-1 (make-scheme-number 0))
;; (print (raise-to s-1 'scheme-number))
;; (print (raise-to s-1 'rational))
;; (print (raise-to s-1 'complex))
;; (print (raise-to 4 'scheme-number))
;; (print (raise-to 4 'rational))
;; (print (raise-to 14 'complex))
;; (print "scheme number done\n\n")

;; (define r-1 (make-rational 1 4))
;; (print (raise-to r-1 'rational))
;; (print (raise-to r-1 'complex))
;; (print (raise-to r-1 'scheme-number)) ;; don't do it
(print "rational number done\n\n")

;; (define z-1 (make-complex-from-real-imag 0 0))
;; (print (raise-to z-1 'complex))
;; (print (raise-to z-1 'rational))
;; (print "complex number done\n\n")

;; (define h1 '(rational complex scheme-number))
;; (define h2 '(scheme-number rational complex))
;; (define h3 '(rational rational))
;; (define h4 '(scheme-number scheme-number))
;; (define h5 '(scheme-number rational))

;; (print (find-highest-type h1))
;; (print (find-highest-type h2))
;; (print (find-highest-type h3))
;; (print (find-highest-type h4))
;; (print (find-highest-type h5))
