#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (same-parity h . t)
    (define (parity fun l lx)
        (cond ((fun l) (cons l (iter lx)))
              (else (iter lx))))
    (define (iter l)
        (if (null? l)
            l
            (parity (if (even? h) even? odd?) (car l) (cdr l))))
    (iter (cons h t)))


(ds (same-parity 1 2 3 4 5 6 7))
(ds (same-parity 2 3 4 5 6 7))

