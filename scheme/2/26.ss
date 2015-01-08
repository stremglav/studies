#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define x (list 1 2 3))
(define y (list 4 5 6))

(d "append x y" (append x y))
(d "cons x y" (cons x y))
(d "list x y" (list x y))
