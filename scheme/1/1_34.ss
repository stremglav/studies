#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (f g)
  (g 2))

(d "function '+ 1'" (f (lambda(x) (+ x 1))))
(d "" (f f))
