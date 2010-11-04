#!/usr/bin/guile -s
!#
(load "utils.ss")

(define dx 0.01)

(define (average a b c) (/ (+ a b c) 2))

(define (compose f g)
    (lambda(x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
        f
        (compose (repeated f (- n 1)) f)))

(define (smooth f)
    (lambda(x) 
        (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (nsmooth f n)
    (repeated (smooth f) n))

(define (nsmooth2 f n)
  ((repeated (lambda (g) (smooth g)) n) f))

(define (nsmooth3 f n)
  ((repeated smooth n) f))

(d "smooth" ((smooth square) 2))

(define (t_nsmooth) ((nsmooth square 2) 1.0))
(define (t_nsmooth2) ((nsmooth2 square 2) 1.0))
(define (t_nsmooth3) ((nsmooth3 square 2) 1.0))

(time_test t_nsmooth)
(time_test t_nsmooth2)
(time_test t_nsmooth3)

