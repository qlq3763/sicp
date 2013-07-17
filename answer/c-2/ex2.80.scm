;; ;;;; add to scheme-number-package
;; (define (=zero? x)
;;   (= x 0))

;; (put '=zero? '(scheme-number) =zero?)

;; ;;;; add to rational-package
;; (define (=zero? x)
;;   (= (number x) 0))

;; (put '=zero? '(rational) =zero?)

;; ;;;; add to complex-package
;; (define (=zero? x)
;;   (and (= (real-part x) 0)
;; 	   (= (imag-part x) 0)))

;; (put '=zero? '(complex) =zero?)

(define (=zero? x)
  (apply-generic '=zero? x))

(define s-1 (make-scheme-number 0))
(define s-2 (make-scheme-number 4))
;; (define s-3 (make-scheme-number -4))
(print (=zero? s-1))
(print (=zero? s-2))
(print (=zero? 0))
(print (=zero? 4))
;;(print (equ? 2 4))
(print "scheme-num done!")

(define r-1 (make-rational 0 4))
(define r-2 (make-rational 1 2))
;;(define r-3 (make-rational 2 3))
(print (=zero? r-1))
(print (=zero? r-2))
(print "rational number done!")

(define z-1 (make-complex-from-real-imag 0 0))
(define z-2 (make-complex-from-mag-ang 5 (atan 4 3)))
(define z-3 (make-complex-from-mag-ang 0 (atan 3 3)))
;; (print (real-part z-2))
;; (print (imag-part z-2))
(print (=zero? z-1))
(print (=zero? z-2))
(print (=zero? z-3))
(print "complex number done!")