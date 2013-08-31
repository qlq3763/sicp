#lang scheme
;;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))
(require "./sicp-plt/sicp.ss")

(define rogers (load-painter "rogers.gif"))
(define half-frame (make-frame (make-vect 0 100)
                               (make-vect 0 99)
                               (make-vect 40 60)))
;; paint with the default frame
(paint rogers)

;; to change the size of the drawed picture, 
;; we need to define a different frame. For now
;; I will use the frame implemented by the support code.
;; I will implement my own version later.

;; in the support code, paint always use the default frame.
;; use transform-painter to get different frame from the default one.

;; 1/4 size
;; note the order of edge1 and edge2 two matters, to ilustrate this,
;; one-quarter2 is defined.
(define org (make-vect 0 0)) 
(define edg1 (make-vect 0.5 0))
(define edg2 (make-vect 0 0.5))
(define one-quarter (transform-painter org edg1 edg2))
(define one-quarter-v2 (transform-painter org edg2 edg1))

(paint (one-quarter rogers))
(paint (one-quarter-v2 rogers))

;; From here, I think I could move on doing the exercises
           