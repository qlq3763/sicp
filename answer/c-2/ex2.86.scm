;;;;;;;;;;;;;; generic arithmetic procedures ;;;;;;;;;;;;;;;;;;;
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

;;;;;;;;;;;;; scheme numerber package ;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (define (=zero? x)
	(= x 0))
  (define (scheme-number->rational n)
	(make-rational n 1))

  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) =zero?)
  (put 'raise '(scheme-number) scheme-number->rational)
  (put 'sine '(scheme-number) ;; change here
	   (lambda (x) (tag (sin x))))
  (put 'cosine '(scheme-number)
	   (lambda (x) (tag (cos x))))
  (put 'atang '(scheme-number)
	   (lambda (x) (tag (atan x)))) 
  (put 'sqrt-g '(scheme-number)
	   (lambda (x) (tag (sqrt x))))
  (put 'square-g '(scheme-number)
	   (lambda (x) (tag (square x))));; end here
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

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
	(print "rational")
	(= (numer x) 0))
  (define (rational->complex r)
	(make-complex-from-real-imag (/ (numer r) (denom r)) 0))
  (define (drop x)
	(round (/ (numer x) (denom x))))

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
  (put 'raise '(rational) rational->complex)
  (put 'drop '(rational) drop)
  (put 'sine '(rational)  ;; change here
	   (lambda (x) (sin (/ (numer x) (denom x)))))
  (put 'cosine '(rational)
	   (lambda (x) (cos (/ (numer x) (denom x)))))
  (put 'atang '(rational)
	   (lambda (x) (atan (/ (numer x) (denom x)))))
  (put 'sqrt-g '(rational)
	   (lambda (x) (sqrt (/ (numer x) (denom x)))))
  (put 'square-g '(rational)
	   (lambda (x) (tag (make-rat (square (numer x))
								  (square (denom x))))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;;;;;;;;;;;;;; complex number package ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (equ-complex? x y)
	(and (equ? (real-part x) (real-part y))
		 (equ? (imag-part x) (imag-part y))))

  (define (=zero-complex? x)
	(and (=zero? (real-part x))
		 (=zero? (imag-part x))))
  (define (drop x)
	(round (real-part x)))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'equ? '(complex complex) equ-complex?)
  (put '=zero? '(complex) =zero-complex?)
  (put 'drop '(complex) drop)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;;;;;;;;;; rectangular-representented complex number ;;;;;;;;;;;;;;;;
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt-g (add (square-g (real-part z))
				 (square-g (imag-part z)))))
  (define (angle z)
    (atang (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (consine (mul r (cosine a)) (mul r (sine a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;;;;;;;;;;;;;; ploar-represented complex number ;;;;;;;;;;;;;;;;;;;
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt-g (add (square-g x) (square-g y)))
          (atang y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;;;;;;;;;;;;; generic selectors ;;;;;;;;;;;;;;
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;;;;;;;;;;;;;; new added here ;;;;;;;;;;;;;;;;;;;;;;
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (atang x) (apply-generic 'atang x))
(define (sqrt-g x) (apply-generic 'sqrt-g x))
(define (square-g x) (apply-generic 'square-g x))

(define (equ? x y)
  (apply-generic 'equ? x y))

(define (=zero? x)
  (apply-generic '=zero? x))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (if (need-drop? op) 
			  (drop (apply proc (map contents args)))
			  (apply proc (map contents args))) 
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
		  (if (equ? dropped x)
			  (project dropped)
			  x))
		x)))

(define (drop x)
  (project x))
;;;;;;; install ;;;;;;;;;;;;;;;;;;;;;;;;;;
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)

(define s-1 (make-scheme-number 0))
(define s-2 (make-scheme-number 4))
;; (print (=zero? s-1))
;; (print (=zero? s-2))
;; (print (equ? s-1 0))
;; (print (equ? s-1 s-2))
;; (print (sine s-1))
;; (print (sine 0))
;; (print (atang 0))
;; (print "sss done\n\n")

(define r-1 (make-rational 0 4))
(define r-2 (make-rational 4 1))
;; (print (=zero? r-1))
;; (print (=zero? r-2)) 
;; (print (equ? r-1 0))
;; (print (equ? r-1 r-2))
;; (print (sine r-1))
;; (print (cosine r-1))
;; (print (atang r-1))
;; (print "rrrr done\n\n")

(define z-1 (make-complex-from-real-imag r-1 r-2))
(define z-2 (make-complex-from-real-imag r-2 r-1))
(define z-3 (make-complex-from-mag-ang r-1 r-2))
(define z-4 (make-complex-from-mag-ang r-2 r-1))
;; (print (=zero? z-1))
;; (print (=zero? z-2))
;; (print (equ? z-2 4))
;; (print (equ? z-1 z-2))
;; (print (equ? z-2 r-2))
(newline)
(print (add z-1 z-2))
(print (add r-1 r-2))
(print (add z-2 -4))
(print (add z-1 r-2))
;; add and sub for regutangular complex numbers is ok.

;; can not do this because I didn't implemnet real-number 
;; (print (real-part z-3)) ;; this one will get a error, because
;; consine will not get an integer(actually get a "real number"
;; and my current package can not deal with "real rational"
;; (print (=zero? z-3))
;; (print (=zero? z-4))
