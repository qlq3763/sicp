;; display the content following by a new line
(define (print content)
  (display content)
  (display "\n")
  true) ;; just don't like "unspecified return value"