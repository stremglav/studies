#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mul a b)
        ( cond  ((or (= b 0) (= a 0)) 0)
                (else  (if (even? b)
                            (double (mul a (halve b)))
                            (+ (mul a (- b 1)) a)))))

(d "mul" (map mul (cl 0 10) (cl 0 10)))
