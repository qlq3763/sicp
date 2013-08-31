(define (my-reverse-1 sequence)
  ;; x means car, y means result of rest
  (fold-right (lambda (x y) 
				(append y (list x)))
			  '()
			  sequence))

(define (my-reverse-2 sequence)
  ;; x means result of previous, y means next element needed to add 
  ;; to already reversed list consisting of all elements before y
  (fold-left (lambda (x y) 
			   (append (list y) x))
			 '()
			 sequence))
;; (define (my-reverse l)
;;   (if (null? l)
;;       l
;;       (append (my-reverse (cdr l))
;; 	      (list (car l)))))

;; (newline)
;; (display l-1)

;; (newline)
;; (display (my-reverse l-1))
(define seq (list 1 3 3 4))
(print (my-reverse-1 seq))
(print (my-reverse-2 seq))