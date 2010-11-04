#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (cons2 x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Аргумент не 0 или 1 -- CONS" m))))
   )

(define (car2 z) (z 0))
(define (cdr2 z) (z 1))

(define ddd (cons2 4 7))

(define (cons3 x y)
  (lambda (m) (m x y)))
(define (car3 z)
  (z (lambda (p q) p)))
(define (cdr3 z)
  (z (lambda (p q) q)))

(ds (car3 (cons3 2 3)))
(ds (cdr3 (cons3 2 3)))
