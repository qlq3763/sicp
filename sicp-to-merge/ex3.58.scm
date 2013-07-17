(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(show-stream (expand 1 7 10) 20) ; 1 4 2 8 5 7, repeat
(show-stream (expand 3 8 10) 20) ; 3 7 5 0 0 0 ...

;; The stream is the fractional part computed from a long division:
;; num divided by den with radix as the base. For example: 
;; 1 / 3, with base 10. 

;; See also: 
;; http://wqzhang.wordpress.com/2009/08/09/sicp-exercise-3-58/
;; http://eli.thegreenplace.net/2007/11/05/sicp-sections-351-352/
