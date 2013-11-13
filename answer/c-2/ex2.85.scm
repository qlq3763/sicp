;; hierarchy: scheme-number(as integer) -> rational -> complex
;; add to rational 
;; (define (drop x)
;;   (round (/ (numer x) (denom x))))

;; (put 'drop '(rational) drop)

;; add to complex
;; (define (drop x)
;;   (real-part x))

;; (put 'drop '(complex) drop)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (if (need-drop? op) ;; add here
			  (drop (apply proc (map contents args)))
			  (apply proc (map contents args))) ;; end here
		  (if (not (check-type-list type-tags))
			  (error "No method for these types"
					 (list op type-tags))
			  (apply apply-generic
					 (append (list op)
							 (raise-to-highest args
											   (find-highest-type type-tags)))))))))

(define need-drop-op-list '(add sub mul div))

(define (need-drop? op)
  (memq op need-drop-op-list))

(define (project x)
  (let ((d-p (get 'drop (list (type-tag x)))))
	(if d-p
		(let ((dropped (d-p (contents x))))
		  (if (equ? dropped x) ;; equ? will raise dropped
			  (project dropped)
			  x))
		x)))

(define (drop x)
  (project x))
			  



;;;;;;;;;;;;;;;;;;;;;; test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define s-1 (make-scheme-number 0))
(define s-2 (make-scheme-number 4))
(define s-3 (make-scheme-number -4))
(newline)
(print (drop s-3))
(print (drop 14))
(print "scheme-num done\n\n")

(define r-1 (make-rational 0 4))
(define r-2 (make-rational 1 2))
(define r-3 (make-rational 2 1))
;; (print (equ? 2 r-3))
;; (print (drop r-1))
;; (print (drop r-2))
;; (print (drop r-3))
(print (add r-1 r-3))
(print (add -4 r-3))
(print (add r-3 s-3))
(print (add r-2 2))
(print "rational number done!\n\n")

(define z-1 (make-complex-from-real-imag 2 0))
(define z-2 (make-complex-from-real-imag 5 3))
(define z-3 (make-complex-from-mag-ang 5 (atan 4 3)))
(define z-4 (make-complex-from-mag-ang 0 (atan 3 3)))
;; (print (drop z-1))
;; (print (drop z-2))
;; (print (drop z-3))
;; (print (drop z-4))
(print (add z-1 z-2))
(print (add z-1 z-1))
(print (add z-1 4))
(print (add z-2 4))
(print (add z-2 r-3))
(print (add z-1 r-3))
(print "complex number done!\n\n")