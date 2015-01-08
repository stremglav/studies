#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (tree-map1 fun tree)
    (cond ((null? tree) tree)
          ((not (pair? tree)) (fun tree))
          (else (cons (tree-map1 fun (car tree)) 
                      (tree-map1 fun (cdr tree))))))

(define (tree-map fun tree)
    (map (lambda(x) 
            (if(pair? x)
                (tree-map fun x)
                (fun x)))
         tree))

(define (square-tree tree) (tree-map square tree))
(define (square-tree1 tree) (tree-map1 square tree))

(define x (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))

(d "x" x)
(d "square-tree x" (square-tree x))
(d "square-tree1 x" (square-tree1 x))
