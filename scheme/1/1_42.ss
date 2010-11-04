#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (inc x) (1+ x))

(define (compose f g) 
    (lambda(x) (f (g x))))

(d "compose" ((compose square inc) 6))
(d "compose" ((compose inc square) 6))
