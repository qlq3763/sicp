#lang scheme
(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define rogers (load-painter "fovnder.gif"))

(define (print val)
  (display val)
  (newline))
; Exercise 2.44
;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (- n 1))))
;        (below painter (beside smaller smaller)))))



; Exercise 2.45
; split with applying second op to smaller painter and first op
; to identity painter and the result got from the previous step
(define (split first-op second-op)
  (define (n-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (n-split painter (- n 1))))
          (first-op painter (second-op smaller smaller)))))
  n-split)
(define right-split (split beside below))
(define up-split (split below beside))

;Exercise 2.46
;(define (make-vect x y)
;  (list x y))
;
;(define (vect-xcor v)
;  (car v))
;
;(define (vect-ycor v)
;  (cadr v))
;
;(define (add-vect v1 v2)
;  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
;             (+ (ycor-vect v1) (ycor-vect v2))))
;
;(define (sub-vect v1 v2)
;  (make-vect (- (xcor-vect v1) (xcor-vect v2))
;             (- (ycor-vect v1) (ycor-vect v2))))
;
;(define (scale-vect v x)
;  (make-vect (* (xcor-vect v) x)
;             (* (ycor-vect v) x)))
;
;(define v1 (make-vect 4 -4))
;(define v2 (make-vect -4 4))
;(print v1)
;(print v2)
;(print (add-vect v1 v2))
;(print (sub-vect v1 v2))
;(print (scale-vect v1 -1))

; Exercise 2.47
;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))
;
;(define (frame-origin f)
;  (car f))
;
;(define (frame-edge1 f)
;  (cadr f))
;
;(define (frame-edge2 f)
;  (caddr f))

;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))
;
;(define (origin-frame f)
;  (car f))
;
;(define (edge1-frame f)
;  (cadr f))
;
;(define (edge2-frame f)
;  (cddr f))

;(define frame (make-frame 0 4 -4))
;(print frame)
;(print (origin-frame frame))
;(print (edge1-frame frame))
;(print (edge2-frame frame))

; Exercise 2.48
;(define (make-segment start-vect end-vect)
;  (list start-vect end-vect))
;
;(define (segmen-start s)
;  (car s))
;
;(define (segment-end s)
;  (cadr s))

;(define s1 (make-segment v1 v2))
;(print s1)
;(print (start-segment s1))
;(print (end-segment s1))

; Exercise 2.49
; a
(define origin (make-vect 0 0))
(define edge1 (make-vect 4 0))
(define edge2 (make-vect 0 4))
(define frame (make-frame origin edge1 edge2))
;(define (frame-outline frame)
;  (let ((origin (make-vect 0 0))
;        (x-cor (make-vect 1 0))
;        (y-cor (make-vect 0 1))
;        (diagonal (make-vect 1 1)))
;    (segments->painter (list (make-segment origin x-cor)
;                           (make-segment origin y-cor)
;                           (make-segment x-cor diagonal)
;                           (make-segment y-cor diagonal)))))

(define nil '())
 
(define one 0.99)
 
;; (define origin (make-vect 0 0))
 
(define lower-right (make-vect one 0))
 
(define upper-left (make-vect 0 one))
 
(define upper-right (make-vect one one))

(define (outline frame)
  ((segments->painter (list (make-segment origin lower-right)
                            (make-segment lower-right upper-right)
                            (make-segment upper-right upper-left)
                            (make-segment upper-left origin)))
   frame))

(paint (outline frame))

;(define ppp (frame-outline frame))

;(print (make-segment 2 3))
;(print (cons 2 3))
;(for-each print (list 2 4))
;(paint (segments->painter (list (make-segment origin edge1)))
;(paint rogers)
;(paint (right-split rogers 4))
;(paint (corner-split rogers 4))
;(paint (square-limit rogers 1))