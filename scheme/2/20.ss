#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (same-parity h . t)
    (define fun (if (even? h) even? odd?))
    (define (iter l)
        (cond 
            ((null? l) l)
            ((fun (car l)) (cons (car l) (iter (cdr l))))
            (else (iter (cdr l)))))
    (iter (cons h t)))


(d "(same-parity 1 2 3 4 5 6 7)" (same-parity 1 2 3 4 5 6 7))
(d "(same-parity 2 3 4 5 6 7)" (same-parity 2 3 4 5 6 7))

