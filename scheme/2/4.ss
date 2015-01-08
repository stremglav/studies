#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (cons3 x y)
  (lambda (m) (m x y)))
(define (car3 z)
  (z (lambda (p q) p)))
(define (cdr3 z)
  (z (lambda (p q) q)))

(ds (car3 (cons3 2 3)))
(ds (cdr3 (cons3 2 3)))
