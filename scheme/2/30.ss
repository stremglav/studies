#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)) )))

(define (square-list2 items)
  (map square items ))

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree2 tree)
    (map (lambda(x) 
            (if(pair? x)
                (square-tree2 x)
                (square x)))
         tree))

(ds (square-tree
        (list 1
            (list 2 (list 3 4) 5)
            (list 6 7))))


(ds (square-tree2
        (list 1
            (list 2 (list 3 4) 5)
            (list 6 7))))
