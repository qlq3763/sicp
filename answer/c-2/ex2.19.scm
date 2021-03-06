(define (cc amount coin-values)
  (define (first-denomination l)
    (car l))

  (define (except-first-denomination l)
    (cdr l))

  (define (no-more? l)
    (null? l))

  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define us-coins-v2 (list 1 5 10 25 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(newline)
(print (cc 100 us-coins))
(print (cc 100 us-coins-v2))
(print (cc 50 uk-coins))