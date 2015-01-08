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

(d "smooth" ((smooth square) 2))

(d "nsmooth" ((nsmooth square 2) 2))
(d "nsmooth_test" ((smooth square) ((smooth square) 2)))
(d "nsmooth2" ((nsmooth2 square 2) 2))
(d "nsmooth2_test" ((smooth (smooth square)) 2))
