(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
;; 1
(display-line sum)

(define y (stream-filter even? seq))
;; 6
(display-line sum)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
;; 10
(display-line sum)

(stream-ref y 7)
;; sum: 136
;; print: 136
(display-line sum)

(display-stream z)
;; sum: 210
;; print: 10, 15, 45, 55, 105, 120, 190, 210
(display-line sum)

;; These responses will differ, although the order of elements adding to
;; sum is the same, but some elements of seq will be add to sum multiple 
;; timea. While with memo-proc, each element will be just added to sum only
;; once.

;; see also:http://wqzhang.wordpress.com/2009/08/08/sicp-exercise-3-52/
