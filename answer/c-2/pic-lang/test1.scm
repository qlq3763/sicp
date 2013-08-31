#lang racket
(require 2htdp/image)

(define rogers (bitmap "./rogers.gif"))
(define pix-rogers (image->color-list rogers))
(define rev-pix (reverse pix-rogers))
(define rev-rogers (color-list->bitmap rev-pix 128 128))