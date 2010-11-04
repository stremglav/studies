#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (square x) (* x x))

(define (pow b n)
    (define (fast-expt b n a)
        (cond ((= n 0) a)
              ((even? n) (fast-expt (square b) (/ n 2) a))
              (else (fast-expt b (- n 1) (* b a)))))

    (fast-expt b n 1))

(map (lambda(x) (d  x (pow 2 x))) (list 1 2 3 4 5 6 7 8 9) )
