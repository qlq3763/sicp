(define (make-interval lower upper)
  (cons lower upper))

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (print-interval i)
  (newline)
  (display "(")
  (display (lower-bound i))
  (display "--")
  (display (upper-bound i))
  (display ")"))

(define i (make-interval 4 14))
(display i)