
(define s (cons-stream 1 (add-streams s s)))
;; 2 ^ n, for n >= 0