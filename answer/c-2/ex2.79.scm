;; ;; add to install-scheme-number-package
;; (put 'equ? '(scheme-number scheme-number) =)

;; ;; add to install-rational-package
;; (define (equ? x y)
;;   (and (= (numer x) (numer y))
;; 	   (= (denom x) (denom y))))

;; (put 'equ? '(ratiional rational) equ?)

;; ;; add to install-complex-package
;; (define (equ? x y)
;;   (and (= (real-part x) (real-part y))
;; 	   (= (imag-part x) (imag-part y))))

;; (put 'equ? '(complex complex) equ?)
(define (equ? x y)
  (apply-generic 'equ? x y))

(define s-1 (make-scheme-number 4))
(define s-2 (make-scheme-number 4))
(define s-3 (make-scheme-number -4))

(newline)
(print (equ? s-1 s-2))
(print (equ? s-1 s-3))
(print (equ? 2 2))
(print (equ? 2 4))
(print "scheme-num done!")

(define r-1 (make-rational 2 4))
(define r-2 (make-rational 1 2))
(define r-3 (make-rational 2 3))
(print (equ? r-1 r-2))
(print (equ? r-1 r-3))
(print "rational number done!")

(define z-1 (make-complex-from-real-imag 3 4))
(define z-2 (make-complex-from-mag-ang 5 (atan 4 3)))
(define z-3 (make-complex-from-mag-ang 5 (atan 3 3)))
(print (real-part z-2))
(print (imag-part z-2))
(print (equ? z-1 z-1))
(print (equ? z-1 z-2))
(print (equ? z-2 z-3))
(print "complex number done!")