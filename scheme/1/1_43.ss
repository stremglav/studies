#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (inc x) (1+ x))

(define (compose f g)
    (lambda(x) (f (g x))))

(define (repeated f n)
    (if (= (- n 1) 0)
        f
        (compose (repeated f (- n 1)) f)))


(define (irepeated f n)
    (define (iter a acc)
        (if (= a n)
            acc
            (iter (+ a 1) (compose f acc))))
    (iter 1 f))

(d "repeated" ((repeated square 2) 5))
(d "repeated" ((repeated square 3) 5))
(d "repeated" ((repeated inc 3) 5))
(d "repeated" ((repeated inc 4) 5))

(d "repeated2" ((irepeated square 2) 5))
(d "repeated2" ((irepeated square 3) 5))
(d "repeated2" ((irepeated inc 3) 5))
(d "repeated2" ((irepeated inc 4) 5))
