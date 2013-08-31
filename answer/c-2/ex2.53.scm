(newline)
(print (list 'a 'b 'c)) ; (a, b, c)

(print (list (list 'george))) ;((george))
(print (cdr '((x1 x2) (y1 y2)))) ;((y1 y2))

(print (cadr '((x1 x2) (y1 y2)))) ;(y1 y2)
(print (pair? (car '(a short list)))) ; false
(print (memq 'red '((red shoes) (blue shoes)))) ; false

(print (memq 'red '(red shoes blue shoes))) ; (red shoes blue shoes)
