(define (make-from-mag-ang mag ang)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* mag (cos ang)))
          ((eq? op 'imag-part) (* mag (sin ang)))
          ((eq? op 'magnitude) mag)
          ((eq? op 'angle) ang)
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)