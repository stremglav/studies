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

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '() 
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if(null? set)
     set
     (if (null? (cdr set))
         (car set)
         (successive-merge 
          (adjoin-set 
           (make-code-tree (car set) (cadr set))
           (cddr set))))))

(define test5-alphabet '((a 1) (b 2) (c 4) (d 8) (e 16)))
(define huffman5-tree (generate-huffman-tree test5-alphabet))


(define test10-alphabet '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512)))
(define huffman10-tree (generate-huffman-tree test10-alphabet))