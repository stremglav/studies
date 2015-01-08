#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree2 tree)
    (map (lambda(x) 
            (if(pair? x)
                (square-tree2 x)
                (square x)))
         tree))

(define x (list 1
            (list 2 (list 3 4) 5)
            (list 6 7)))

(d "x" x)
(d "square-tree x" (square-tree x))
(d "square-tree2 x" (square-tree2 x))
