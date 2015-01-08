#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (fringe l)
    (cond ((null? l) l)
          ((not (pair?  l)) (list l))
          (else (append (fringe (car l)) (fringe (cdr l))))))

(define x (list (list 1 2) (list 3 4)))

(d "(list (list 1 2) (list 3 (list 4 5) 6 (list 7 8)) (list 9 10))"
    (fringe (list (list 1 2) (list 3 (list 4 5) 6 (list 7 8)) (list 9 10))))
(d "x" x)
(d "fringe x" (fringe x))
(d "fringe (list x x)" (fringe (list x x)))
