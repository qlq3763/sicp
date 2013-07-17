(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define seqs '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(accumulate-n + 0 seqs)