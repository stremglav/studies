#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (subsets s)
    (if (null? s)
        (list s)
        (let ((rest (subsets (cdr s))))
             (append rest (map (lambda(sx) (cons (car s) sx)) rest)))))

(ds (subsets (list 1 2 3)))


