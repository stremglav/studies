#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mul a b)
    (define (mul-iter a b acc)
        ( cond  ((= b 0) 0)
                ((= b 1) (+ acc a))
                (else  (if (even? b)
                            (mul-iter (double a) (halve b) acc)
                            (mul-iter a (- b 1) (+ a acc))))))
    (mul-iter a b 0)
)

(d "mul" (map mul (cl 0 10) (cl 0 10)))
