#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (fringe l)
    (cond ((null? l) l)
          ((not (pair?  (car l))) l)
          (else (append (fringe (car l)) (fringe (cdr l))))
          
    )
)

(define x (list (list 1 2) (list 3 4)))

(ds (fringe (list (list 33 23) (list 72 149) (list 99 34))))
(ds (fringe x))
(ds (fringe (list x x)))
