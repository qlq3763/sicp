(define (apply-generic op . args)
  (define (same-type? lst)
	(let ((type (car lst)))
	  (every (lambda (x) (eq? x type)) lst)))

  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
		  (if (same-type? type-tags)
			  (error "No method for these types"
					 (list op type-tags))
			  (let ((coercion-args (coercion args 0)))
				(if coercion-args
					(apply 
					 apply-generic (append (list op) coercion-args))
					)))))))

(define (try-coercion target-type args-list result)
  (if (null? args-list)
	  result
	  (let ((a1 (car args-list)))
		(let ((t1 (type-tag a1)))
		  (cond ((eq? target-type t1)
				 (try-coercion target-type
							   (cdr args-list)
							   (append result (list a1))))
				((get-coercion t1 target-type)
				 (try-coercion target-type
							   (cdr args-list)
							   (append result
									   (list ((get-coercion t1 target-type) a1)))))
				(else result))))))

(define (coercion args-list nth)
  (if (>= nth (length args-list))
	  false
	  (let ((try-list (try-coercion 
					   (type-tag (list-ref args-list nth)) 
								 args-list 
								 '())))
		(if (= (length args-list) (length try-list))
			try-list
			(coercion args-list (+ nth 1))))))


(define (put-coercion from-type target-type coercion-func)
  (put from-type target-type coercion-func))

(define (get-coercion from-type target-type)
  (get from-type target-type))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))


(put-coercion 'scheme-number 'complex scheme-number->complex)

(define s-1 (make-scheme-number 0))
(define s-2 (make-scheme-number 4))

(define r-1 (make-rational 0 4))

(define z-1 (make-complex-from-real-imag 0 0))
(define z-2 (make-complex-from-mag-ang 5 (atan 4 3)))
(define z-3 (make-complex-from-mag-ang 0 (atan 3 3)))

(print (try-coercion 'complex (list -4 s-1 s-2 z-1 z-2 r-1 z-3) '()))
(print (try-coercion 'complex (list -4 s-1 s-2 z-1 z-2 z-3) '()))

(print (coercion (list -4 s-1 s-2 z-1 z-2 r-1 z-3) 0))
(print (coercion (list -4 s-1 s-2 z-1 z-2 z-3) 0))

(print (add z-1 s-1))
(print (add s-1 z-1))
(print (add z-2 -1))
(print (add -1 z-2))
;; A situation where this strategy is not sufficiently general: we are
;; trying to coerce the arguments only to types that are present in the
;; call, and so can miss other types.

;; Consider the example in Figure 2.26 in the book. Suppose we have a
;; call with these types: (kite quadrilateral). Since kite can be coerced
;; into quadrilateral, everything works as expected.

;; But now suppose we have a call with: (triangle kite
;; quadrilateral). Going over each of these types and trying to
;; coerce them to each other won’t work. But they can all be coerced 
;; into polygon. This demonstrates the flaw of this method. What we
;; should be really going is finding some common “ancestor” type for all
;; the types we work on.

