#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (paskal n)
(
    (define (cr_data data)
        ( map (lambda(a b) (+ a b)) (list-tail data 1)
                                    (list-head data (- (length data) 1))))
    (cond 
        ((= n 0) (list 1))
        ((= n 1) (list 1 1))
        ((> n 1) (append (list 1) (cr_data (paskal (- n 1))) (list 1)) )
    )
)

(show_list paskal)
