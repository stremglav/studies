#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (reverse l)
    (if (null? l)
        l
        (append (reverse (cdr l))  (list (car l)))))

(define (deep-reverse l)
    (if (pair? l)
        (reverse (map (lambda(x) (deep-reverse x)) l))
        l))

(define x (list 0 (list 1 2) (list 3.0 (list 3.1 3.2) (list 4.1 4.2 (list 4.31 4.32))) 5.0 (list 5 6)))

(d "x" x)
(d "reverse x" (reverse x))
(d "deep-reverse x" (deep-reverse x))
