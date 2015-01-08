#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (inc x) (1+ x))

(define (compose f g)
    (lambda(x) (f (g x))))

(define (repeated f n)
    (if (= (1- n) 0)
        f
        (compose (repeated f (1- n)) f)))


(define (irepeated f n)
    (define (iter a acc)
        (if (= a n)
            acc
            (iter (1+ a) (compose f acc))))
    (iter 1 f))

(d "repeated" ((repeated square 2) 5))
(d "repeated2" ((irepeated square 2) 5))
