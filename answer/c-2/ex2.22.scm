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

;; version 1
(define (square-list-v1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; version 2
(define (square-list-v2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;; the correct version
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer (list (square (car things)))))))
  (iter items '()))

;; defined in ex2.21.scm
;; (define l-3 (list 1 2 3 4 14))
;; (define l-4 (list -1 1 4 14 -4))

(newline)
(display "original list:\n")
(display l-3)
(newline)
(display l-4)

;; version 1
(newline)
(display "square-lsit version 1:\n")
(display (square-list-v1 l-3))
(newline)
(display (square-list-v1 l-4))

;; version 2
(newline)
(display "square-lsit version 2:\n")
(display (square-list-v2 l-3))
(newline)
(display (square-list-v2 l-4))

;; correct version
(newline)
(display "suqare-list correct version:\n")
(display (square-list-iter l-3))
(newline)
(display (square-list-iter l-4))

