#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (inc x) (1+ x))

(define (double f) 
    (lambda(x) (f (f x))))

(d "double" ((double inc) 1))

(d "mega-vir" (((double (double double)) inc) 5))
