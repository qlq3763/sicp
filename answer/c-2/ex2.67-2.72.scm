
;; representing

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;; EXERCISE 2.67

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
;; (print sample-tree)
;; (print sample-text)
;; Message: (a d a b b c a)

;; EXERCISE 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (member? x seq)
  (pair? (memq x seq)))

;; (print (member? 4 '(1 2 4)))
;; (print (member? 3 '(1 2 4)))
(define (encode-symbol symbol tree)
  (define (encode-valid-symbol symbol tree)
    (if (leaf? tree) ; if something wrong, should not reach here
	'()
	(let ((left-symbols (symbols (left-branch tree))))
	  (if (member? symbol left-symbols)
	      (cons 0 (encode-valid-symbol symbol (left-branch tree)))
	      (cons 1 (encode-valid-symbol symbol (right-branch tree)))))))

  (if (member? symbol (symbols tree))
      (encode-valid-symbol symbol tree)
      (error "invalid symbol" symbol)))

(define sample-text (decode sample-message sample-tree))
;; (print (equal? (encode sample-text sample-tree) sample-message))

;; EXERCISE 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pair-set)
  (if (= (length pair-set) 1)
      (car pair-set)
      (let ((merged (make-code-tree (car pair-set) (cadr pair-set)))
	    (rest (cddr pair-set)))
	(successive-merge (adjoin-set merged rest)))))

(define test-output (generate-huffman-tree '((a 4) (b 2) (c 1) (d 1))))
;; (print test-output)
;; (print sample-tree)
;; (print (equal? sample-tree test-output))

;; Exercise 2.70
(define rock-song-pairs '((a 2) (boom 1) (get 2) (job 2) 
			  (na 16) (sha 3) (yip 9) (wah 1)))

(define rock-song-lyrics '(Get a job
			       Sha na na na na na na na na
			       Get a job
			       Sha na na na na na na na na 
			       Wah yip yip yip yip yip yip yip yip yip
			       Sha boom))

(print rock-song-pairs)
(print rock-song-lyrics)

(define rock-song-tree (generate-huffman-tree rock-song-pairs))
(print rock-song-tree)

(define encoded-song (encode rock-song-lyrics rock-song-tree))
(print encoded-song)
(print (length encoded-song))

84 bits are required for the encoding.
To encode 8 different symbols, we need 3 bits. And there are 
36 symbols in the song, so the smallest number of bits that
wouldbe need to encode this song is we used a fixed-length
code for the eight-symbol is 36 * 3 = 108.