(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define c (make-from-mag-ang 2 (/ 3.14159265 4)))

(newline)
(print (c 'real-part))
(print (c 'imag-part))
(print (c 'magnitude))
(print (c 'angle))
