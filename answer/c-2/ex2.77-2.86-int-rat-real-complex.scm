;; a number system with four types: integer, rational, real, complex. 
;; These four types forms a type tower, as figure 2.25 from the textbook.
;; Here real number means decimal fractional number.

;; The problems: 
;; 1. (sqrt-g integer) may produce real or integer, 
;;    the othe procedure: sine, cosine, atang, may(vely unlikely) have this problem
;;    but I will just take them(may be integer) as real.
;; 2. (sqrt-g rational) may produce real or ratinal,

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Integer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-integer-package)
  (define (tag x)
    (attach-tag 'integer x))
  (define (tag-real x)
	(attach-tag 'real x))
  (define (=zero? x)
	(= x 0))
  (define (integer->rational n)
	(make-rational n 1))

  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) ;; "int / int" may be rational, mistake here
       (lambda (x y) (if (= (remainder x y) 0)
						 (tag (/ x y))
						 (make-rational x y))))
							 
						 
  (put 'make 'integer
       (lambda (x) (tag x)))
  (put 'equ? '(integer integer) =)
  (put '=zero? '(integer) =zero?)
  (put 'raise '(integer) integer->rational)
  (put 'sine '(integer) 
	   (lambda (x) (tag-real (sin x))))
  (put 'cosine '(integer)
	   (lambda (x) (tag-real (cos x))))
  (put 'atang '(integer integer)
	   (lambda (x y) (tag-real (atan x y)))) 
  (put 'sqrt-g '(integer)
	   (lambda (x) (let ((sqt (sqrt x)))
					 (if (integer? sqt)
						 (tag-real sqt)
						 (tag sqt)))))
  (put 'square-g '(integer)
	   (lambda (x) (tag (square x))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Rational ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
	(let ((rst (* 1.0 (/ (numer r) (denom r)))))
	  (if (= rst 0)
		  0.0 ;; here, 0 is integer, 0.0 is real
		  rst)))
  (define (rational->integer x)
	(inexact->exact (round (/ (numer x) (denom x)))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (tag-real x) (attach-tag 'real x))
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
  (put 'sine '(rational)  ;; change here
	   (lambda (x) (tag-real (sin (/ (numer x) (denom x))))))
  (put 'cosine '(rational)
	   (lambda (x) (tag-real (cos (/ (numer x) (denom x))))))
  (put 'atang '(rational rational)
	   (lambda (x y) (tag-real (atan (/ (numer x) (denom x))
									 (/ (numer y) (denom y))))))
  (put 'sqrt-g '(rational)
	   (lambda (x) (let ((nq (sqrt (numer x)))
						 (dq (sqrt (denom x))))
					 (if (and (integer? nq) (integer? dq))
						 (make-rational nq dq)
						 (tag-real (sqrt (/ (numer x) (denom x))))))))
  (put 'square-g '(rational)
	   (lambda (x) (tag (make-rat (square (numer x))
											(square (denom x))))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Real ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (install-real-package)
  (define (tag x)
    (attach-tag 'real x))
  (define (real->complex r)
	(make-complex-from-real-imag r 0))
  (define (real->rational r)
	(define (iter frac ts)
	  ;; the reason, see test cases for real number
	  (let ((product (* frac ts))) 
		(if (integer? product)
			(make-rational (inexact->exact product) ts)
			(iter frac (* ts 10)))))
	(iter r 1))

  ;; should always avoid use "=" on floating nubmbers
  (define (equ? x y)
	(let ((proportion 0.00001))
	  (<= (abs (- x y)) ;; for =zero? this shoud be <=, instead of <
		  (* proportion (+ (abs x) (abs y))))))

  (define (=zero? x)
	(equ? x 0.0))

  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real
       (lambda (x) (tag x)))
  (put 'equ? '(real real) equ?)
  (put '=zero? '(real) =zero?)
  (put 'raise '(real) real->complex)
  (put 'drop '(real) real->rational)
  (put 'sine '(real) 
	   (lambda (x) (tag (sin x))))
  (put 'cosine '(real)
	   (lambda (x) (tag (cos x))))
  (put 'atang '(real real)
	   (lambda (x y) (tag (atan x y)))) 
  (put 'sqrt-g '(real)
	   (lambda (x) (tag (sqrt x))))
  (put 'square-g '(real)
	   (lambda (x) (tag (square x))))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Complex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; There are two levels of abstraction here. 

;; The lower level hides the representation of complex number with the 
;; following interface procedures: real-part, imag-part, magnitude and
;; angle. 

;; The higher level combine these two kinds of complex numbers as one: 
;; complex, instead of rectangular and polar. This is achieved using 
;; the interface procedures provided by the lower level.

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

  (define (=zero-complex? x) ;; ex2.86, real and imag part can be rational..
	(and (=zero? (real-part x))
		 (=zero? (imag-part x))))
  (define (complex->real x)
	(real-part x))
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
  (put 'drop '(complex) complex->real)
  'done)

;; rectangular represententation
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt-g (add (square-g (real-part z))
				 (square-g (imag-part z)))))
  (define (angle z)
    (atan-g (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (mul r (cos-g a)) (* r (sin-g a))))

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

;; ploar representation
(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cos-g (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sin-g (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt-g (add (square-g x) (square-g y)))
          (atan-g y x)))

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

;; generic complex number selectors 
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; constructor
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;;;;;;;;;;;;;;;;;;;;;;;;; apply-generic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
		  (let ((rst (apply proc (map contents args))))
			(if (and (need-drop? op)
					 (get 'drop (map type-tag (list rst))))
				(drop rst)
				rst)) 
		  (if (not (check-type-list type-tags))
			  (error "No method for these types"
					 (list op type-tags))
			  (apply apply-generic
					 (append (list op)
							 (raise-to-highest args
											   (find-highest-type type-tags)))))))))

;; drop
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

;; raise a list of types to the highest one
;;;; hierarchy of types, low -> high
(define hierarchy '(integer rational real complex))

;;;;;;; chcek 
(define (check-type-list type-list)

  (define (same-type? lst)
	(let ((type (car lst)))
	  (every (lambda (x) (eq? x type)) lst)))

  (define (valid-type? type)
	(memq type hierarchy))
  
  (and (not (same-type? type-list))
	   (every valid-type? type-list)))

(define (find-highest-type type-list)
  (define (type-to-position type)
	(- (length hierarchy) (length (memq type hierarchy))))
  (list-ref hierarchy (apply max (map type-to-position type-list))))

(define (raise-to-highest arg-list target-type)  	  
  (define (raise-to arg target-type)
	(if (eq? (type-tag arg) target-type)
		arg
		(raise-to (raise arg) target-type)))

  (map (lambda (arg) (raise-to arg target-type)) arg-list))

;;;;;;;;;;;;;; generic arithmetic procedures ;;;;;;;;;;;;;;;;;;;
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (=zero? x) (apply-generic '=zero? x))
(define (equ? x y) (apply-generic 'equ? x y))

(define (raise x) (apply-generic 'raise x))
;; (define (drop x) (apply-generic 'drop x))

;; can not use sin, cos..., because sine use them for implementation
;; this is an definition-infinite-recursion
(define (sin-g x) (apply-generic 'sine x))
(define (cos-g x) (apply-generic 'cosine x))
(define (atan-g x y) (apply-generic 'atang x y))
(define (sqrt-g x) (apply-generic 'sqrt-g x))
(define (square-g x) (apply-generic 'square-g x))

;;;;;;;;;;;;;;;;;;;;;; Tag ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to handle integer and real number specially

(define (attach-tag type-tag contents)
  (if (number? contents) ;; integer and real
	  contents
	  (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum)
		 (car datum))
		((number? datum)
		 (if (and (exact? datum) ;; can not use integer? (test 3.0)
				  true);;(integer? datum)) ;; for case like 1/2(delete this, see 
			 'integer                      ;; the beginning "The problems" section.)  
			 'real))
		(else error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((pair? datum)
		 (cdr datum))
		((number? datum) ;; integer and real
		 datum)
		(error "Bad tagged datum -- CONTENTS" datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-complex-package)
(install-rectangular-package)
(install-polar-package)

(define i-0 (make-integer 0))
(define i-1 (make-integer 4))
(define i-3 0)
(define i-4 -4)

(print "\ntesting integer start\n")
(assert '(=zero? i-0))
(assert '(=zero? i-3))
(assert '(not (=zero? i-1)))
(assert '(not (=zero? i-4)))

(assert '(equ? i-0 i-3))

(assert '(equ? (add i-1 i-4) i-3))
(assert '(equ? (mul i-1 i-4) -16))
(assert '(equ? (sub i-1 i-0) i-1))
(assert '(equ? (div i-1 i-4) -1))

(assert '(equ? (raise i-1) (make-rational 4 1)))
(print "\ntesting integer done\n")

(define rat-0 (make-rational 0 4))
(define rat-1 (make-rational 2 1))
(define rat-2 (make-rational 1 2))

(print "\ntesting rational start\n")
(assert '(=zero? rat-0))
(assert '(not (=zero? rat-1)))
(assert '(equ? (drop rat-1) 2))

(assert '(equ? (add rat-0 rat-1) 2))
(assert '(equ? (mul rat-1 rat-2) 1))
(assert '(equ? (sub rat-1 rat-2) (make-rational 3 2)))
(assert '(equ? (div rat-2 rat-1) (make-rational 1 4)))
(print "\ntesting rational done\n")

(define real-0 (make-real 0.0))
(define real-1 (make-real 0.5))
(define real-3 0.0)
(define real-4 1.5)

(print "\ntesting real start\n")
(assert '(=zero? real-0))
(assert '(=zero? real-3))
(assert '(not (=zero? real-1)))
(assert '(equ? real-0 real-3))

(assert '(equ? (add real-1 real-4) 2))
(assert '(equ? (mul real-1 real-4) (make-rational 3 4)))
(assert '(equ? (sub real-1 real-4) -1))
;; (assert '(equ? (div real-1 real-4) (make-rational 1 3)))
(print (div real-1 real-4)) 
;; Actually the result is "strange", on my computer 
;; (drop .333333) => (rational 333333 1000000)
;; (drop .3333333) => (rational 8333332500000001 . 25000000000000000)
;; During the process of multiply .3333333 by 10, I think a precision
;; problem happens(precision of floating number). 
;; The following is the code to examine what happens.
;; (define (check-precision r)
;;   (if (integer? r)
;; 	  r
;; 	  (begin (print r)
;; 			 (check-precision (* r 10)))))
;; Run the code, I think while multiplying r by 10, the "error" are
;; accumulating too.

;; One improvement: instead of multiplying r with 10 many times, 
;; accumulate the tens, then multiply once. See the real package,
;; procedure real->rational, for example.

;; real is not exact number, so should not compare with exact number,
;; using exact euqal operator. The test cases are written before I use
;; the inexact comparation for real numbers, and except this div case,
;; the other cases works, so I didn't change them. Theoretically, I should
;; change the cases involving real number caparation.

(assert '(equ? (drop real-1) (make-rational 1 2)))
(print "\ntesting real done\n")

(define c-0 (make-complex-from-real-imag 0 0))
(define c-1 (make-complex-from-real-imag 4 -4))
(define c-3 (make-complex-from-real-imag 4.5 5.5))
(define c-4 (make-complex-from-real-imag rat-0 rat-0))
(define c-5 (make-complex-from-real-imag 0.0 rat-0))

(define c-6 (make-complex-from-mag-ang 0 0))
(define c-7 (make-complex-from-mag-ang 4 -4))
(define c-8 (make-complex-from-mag-ang 4.5 5.5))
(define c-9 (make-complex-from-mag-ang rat-0 rat-0))
(define c-a (make-complex-from-mag-ang 0.0 rat-0))

(print "\ntesting complex start\n")
(assert '(=zero? c-0))
(assert '(=zero? c-6))
(assert '(=zero? c-5)) ;; change complex =zero-complex? to use =zero?
(assert '(=zero? c-a))

(assert '(not (=zero? c-1)))
(assert '(not (=zero? c-8)))
(assert '(equ? c-0 c-6))
(assert '(equ? c-0 c-a))

(print (add c-1 c-3))
(print (add c-1 c-4))
(print (sub c-4 c-5))
(print (sub c-3 c-4))

(print "\n mul \n")
(print (mul c-0 c-1))
(print (mul c-3 c-4))
(print (mul c-3 c-5))
(print (mul c-4 c-5))

(print "\n div \n")
(print (div c-0 c-1))
(print (div c-4 c-1))
(print (div c-5 c-1))

(print "\n mag-ang \n")
(print "\n add \n")
(print (add c-6 c-8))
(print (add c-6 c-9))
(print (sub c-9 c-a))
(print (sub c-8 c-9))

;; (define c-6 (make-complex-from-mag-ang 0 0))
;; (define c-7 (make-complex-from-mag-ang 4 -4))
;; (define c-8 (make-complex-from-mag-ang 4.5 5.5))
;; (define c-9 (make-complex-from-mag-ang rat-0 rat-0))
;; (define c-a (make-complex-from-mag-ang 0.0 rat-0))
(print "\n mul \n")
(print (mul c-7 c-8))
(print (mul c-8 c-7))
(print (mul c-6 c-7))
(print (mul c-9 c-a))
(print (mul c-8 c-a))
(print (mul c-9 c-a))

(print "\n div \n")
(print (div c-6 c-7))
(print (div c-8 c-7))
(print (div c-9 c-7))
(print "\ntesting complex done\n")

;; complex has done a lot of mix-types test, so the mix types test 
;; will be simple

;; (define i-1 (make-integer 4))
;; (define rat-1 (make-rational 2 1))
;; (define real-1 (make-real 0.5))
;; (define c-1 (make-complex-from-real-imag 4 -4))

(print "\n testing mix types start \n")
;; add
(assert '(equ? (add i-1 rat-1) 6))
(assert '(equ? (add i-1 real-1) (make-rational 9 2)))
(assert '(equ? (add i-1 c-1) (make-complex-from-real-imag 8 -4)))
(assert '(equ? (add rat-1 real-1) (make-rational 5 2)))
(assert '(equ? (add rat-1 c-1) (make-complex-from-real-imag 6 -4)))
(assert '(equ? (add real-1 c-1) (make-complex-from-real-imag 4.5 -4)))
(print "add done")

;; sub
(assert '(equ? (sub i-1 rat-1) 2))
(assert '(equ? (sub i-1 real-1) (make-rational 7 2)))
(assert '(equ? (sub i-1 c-1) (make-complex-from-real-imag 0 4)))
(assert '(equ? (sub rat-1 real-1) (make-rational 3 2)))
(assert '(equ? (sub rat-1 c-1) (make-complex-from-real-imag -2 4)))
(assert '(equ? (sub real-1 c-1) (make-complex-from-real-imag -3.5 4)))
(print "sub done")

;; mul
;; (define c-7 (make-complex-from-mag-ang 4 -4))
(assert '(equ? (mul i-1 rat-1) 8))
(assert '(equ? (mul i-1 real-1) 2))
(assert '(equ? (mul i-1 c-7) (make-complex-from-mag-ang 16 -4)))
(assert '(equ? (mul rat-1 real-1) 1))
(assert '(equ? (mul rat-1 c-7) (make-complex-from-mag-ang 8 -4)))
(assert '(equ? (mul real-1 c-7) (make-complex-from-mag-ang 2 -4)))
(print "mul done")

;; div
(assert '(equ? (div i-1 rat-1) 2))
(assert '(equ? (div i-1 real-1) 8))
(assert '(equ? (div i-1 c-7) (make-complex-from-mag-ang 1 4)))
(assert '(equ? (div rat-1 real-1) 4))
(assert '(equ? (div rat-1 c-7) 
			   (make-complex-from-mag-ang (make-rational 1 2) 4)))
(assert '(equ? (div real-1 c-7) 
			   (make-complex-from-mag-ang (make-rational 1 8) 4)))
(print "\n testing mix types done \n")