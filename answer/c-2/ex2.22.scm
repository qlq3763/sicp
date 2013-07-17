;; I think an example can make all these things clear. Suppose we have a
;; list l:(1 2 3 4). 
;; Now let's start with the first version, I just show the value of
;; variable answer at each step:
;; () ->
;; (1) ->
;; (4 1) -> yes, cons works this way
;; (9 4 1) ->
;; (16 9 4 1) -> done.
;; By using cons, the newly computed square is add to the head of list,
;; so this procedure will produce a reverse order of square.

;; The second version:
;; () ->
;; (() . 1) ->
;; ((() . 1) . 4) ->
;; (((() . 1) . 4) . 9) ->
;; ((((() . 1) . 4) . 9) . 16) -> done.
;; Well, this is kind of a mess, but this is how cons works.

(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer (list (square (car things)))))))
  (iter items '()))


(newline)
(display (square-list-iter l-3))

(newline)
(display (square-list-iter l-4))

