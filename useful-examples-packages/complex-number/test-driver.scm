(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;; integer
(define i-1 (make-integer 0))
(define i-2 (make-integer 4))

;; rational 
(define rat-1 (make-rational 0 4))
(define rat-2 (make-rational 4 1))

;; real 
(define r-1 (make-real 0.1))
(define r-2 (make-real 4.5))

;; complex
(define z-1 (make-complex-from-real-imag 0 0))
(print z-1)
(define z-2 (make-complex-from-real-imag 4 3))
(print z-2)
(define z-3 (make-complex-from-mag-ang 0 2))
(print z-3)
(define z-4 (make-complex-from-mag-ang 5 (atan 3 4)))
(print z-4)

;; raise
(print (raise 4))
(print (raise i-2))

(print (raise rat-1))

(print (raise r-1))

(print "raise done\n\n")
;; drop
(print (drop 4))
(print (drop i-2))

(print (drop rat-1))

(print (drop r-1))

(print (drop z-1))
(print (drop z-2))
(print (drop z-3))
(print (drop z-4))

(print "drop done\n\n")

;; integer and rational
(print (add i-1 rat-2))
(print (sub i-1 rat-2))
(print (mul i-1 rat-2))
(print (div i-1 rat-2))
(print (atang i-1 rat-2))
(print (equ? i-1 rat-1))

(print "integer and rational done\n\n")

;; integer and real
(print (add i-1 r-1)) ;; the result dropped to rational is odd
(print (sub i-1 r-1)) 
(print (mul i-1 r-1))
(print (div i-1 r-1))
(print (atang i-1 r-1))
(print (equ? i-1 0.0))

(print "integer and real done\n\n")

;; integer and complex
(print (add i-1 z-1))
(print (sub i-1 z-2))
(print (mul i-1 z-3))
(print (div i-1 z-4))
(print (equ? i-1 z-1))
(print (equ? i-1 z-3))

(print "integer and complex done\n\n")

;; rational and real
(print (add rat-1 r-1))
(print (sub rat-1 r-1))
(print (mul rat-1 r-1))
(print (div rat-1 r-1))
(print (atang rat-1 r-1))
(print (equ? rat-1 0.0))

(print "rational and real done\n\n")

;; rational and complex
(print (add rat-1 z-1))
(print (sub rat-1 z-2))
(print (mul rat-1 z-3))
(print (div rat-1 z-4))
(print (equ? rat-1 z-1))
(print (equ? rat-1 z-3))

(print "rational and complex done\n\n")

;; real and complex
(print (add z-1 r-1))
(print (sub z-2 r-1))
(print (mul z-3 r-1))
(print (div z-4 r-1))
(print (equ? 0.0 z-1))
(print (equ? z-3 0.0))

(print "complex and real done\n\n")

;; complex "complex numbers"
(define z-5 (make-complex-from-real-imag i-1 rat-1))
(define z-6 (make-complex-from-real-imag i-1 r-1))
(define z-7 (make-complex-from-real-imag rat-1 r-1))

(define z-8 (make-complex-from-mag-ang i-1 rat-1))
(define z-9 (make-complex-from-mag-ang i-1 r-1))
(define z-a (make-complex-from-mag-ang rat-2 r-1))

(print (add z-5 z-7))
(print (sub z-6 z-7))
(print (mul z-8 z-a))
(print (div z-9 z-a))

(print "z-z done\n\n")

(print (add z-5 i-2))
(print (sub z-7 rat-2))
(print (mul z-5 r-1))
(print (div z-8 i-2))

(print (equ? z-5 z-8))
(print (equ? z-9 i-1))
(print (equ? z-9 0.0))
(print (equ? z-5 rat-1))
(print (=zero? z-5))
(print (=zero? z-9))