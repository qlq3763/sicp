;;;;;;;;;;; rational numbe package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
	(and (= (numer x) (numer y))
		 (= (denom x) (denom y))))
  (define (=zero? x)
	(= (numer x) 0))
  (define (rational->real r)
	(make-real (exact->inexact (/ (numer r)  (denom r)))))
  (define (rational->integer x)
	(make-integer (round (/ (numer x) (denom x)))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'raise '(rational) rational->real)
  (put 'drop '(rational) rational->integer)
  (put 'sine '(rational)  
	   (lambda (x) (sin (/ (numer x) (denom x)))))
  (put 'cosine '(rational)
	   (lambda (x) (cos (/ (numer x) (denom x)))))
  (put 'atang '(rational rational)
	   (lambda (x y) (atan  (* (numer x) (denom y))
							(* (denom x) (numer y)))))
  (put 'sqrt-g '(rational)
	   (lambda (x) (sqrt (/ (numer x) (denom x)))))
  (put 'square-g '(rational)
	   (lambda (x) (tag (make-rat (square (numer x))
								  (square (denom x))))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


;; test (only for rational)
(install-rational-package)

(define rat-1 (make-rational 0 4))
(define rat-2 (make-rational 4 1))

(print rat-1)
(print rat-2)

;; add
(print (equ? (add rat-1 rat-2) rat-2))
(print "rational add done\n\n")

;; sub
(print (equ? (sub rat-1 rat-2) (make-rational -4 1)))
(print "rational sub done\n\n")

;; mul
(print (=zero? (mul rat-1 rat-1)))
(print "rational mul done\n\n")

;; div
(print (=zero? (div rat-1 rat-2)))
(print "rational div done\n\n")

;; sine
(print (sine rat-1))
(print "rational sine done\n\n")

;; cosine
(print (cosine rat-1))
(print "rational cosine done\n\n")

;; atang
(print (atang rat-1 rat-2))
(print "rational atang done\n\n")

;;sqrt-g
(print (sqrt-g rat-2))
(print "rational sqrt-g done\n\n")

;; square-g
(print (square-g rat-2))
(print "rational square-g done\n\n")