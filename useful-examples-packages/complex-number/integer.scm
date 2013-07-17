;; Most of the code of this package is the same as real number 
;; package. 

;; First, I put integer and real number in one package called
;; scheme numbe package. Then I found that method is messy, so, 
;; now this is the "duplicate-code" version

;; Can not use attach-tag here, see notes for the reason.

;; qlq
;; June 21, 2012

;;;;;;;;;;;;; integer number package ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-integer-package)
  ;; (define (tag x)
  ;;   (attach-tag 'integer x))
  (define (integer->rational n)
	(make-rational n 1))

  ;; interface
  (put 'add '(integer integer) +)
  (put 'sub '(integer integer) -)
  (put 'mul '(integer integer) *)
  (put 'div '(integer integer) /)
  (put 'sine '(integer) sin)
  (put 'cosine '(integer) cos)
  (put 'atang '(integer integer) atan)
  (put 'sqrt-g '(integer) sqrt)
  (put 'square-g '(integer) square)
  (put 'make 'integer (lambda (x) x))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer) zero?)
  (put 'raise '(integer) integer->rational)
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

;; test (only for integers)
(install-integer-package)

(define i-1 (make-integer 0))
(define i-2 (make-integer 4))

(print i-1)
(print i-2)

;; add
(print (equ? (add i-1 i-2) 4))
(print (equ? (add i-1 4) 4))
(print (equ? (add 4 i-1) 4))
(print (equ? (add 4 4) 8))
(print "integer add done\n\n")

;; sub
(print (equ? (sub i-1 i-2) -4))
(print (equ? (sub i-1 4) -4))
(print (equ? (sub 4 i-1) 4))
(print (=zero? (sub 4 4)))
(print "integer sub done\n\n")

;; mul
(print (=zero? (mul i-1 i-2)))
(print (=zero? (mul i-1 4)))
(print (=zero? (mul 4 i-1)))
(print (equ? (mul 4 4) 16))
(print "integer mul done\n\n")

;; div
(print (=zero? (div i-1 i-2)))
(print (=zero? (div i-1 4)))
(print (equ? (div 4 i-2) 1))
(print (equ? (div 4 4) 1))
(print "integer div done\n\n")

;; sine
(print (sine i-1))
(print (sine 0))
(print "integer sine done\n\n")

;; cosine
(print (cosine i-1))
(print (cosine 0))
(print "integer cosine done\n\n")

;; atang
(print (atang i-1 2))
(print (atang 0 2))
(print "integer atang done\n\n")

;;sqrt-g
(print (sqrt-g i-1))
(print (sqrt-g 0))
(print "integer sqrt-g done\n\n")

;; square-g
(print (square-g i-2))
(print (square-g 4))
(print "integer square-g done\n\n")