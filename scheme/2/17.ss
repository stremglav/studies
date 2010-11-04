#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (last-pair l)
    (define (iter ll elem)
        (if (null? ll)
            elem
            (iter (cdr ll) (car ll))
        )
    )
    (iter (cdr l) (car l))
)


(define (last-pair2 l)
    (if (null? (cdr l))
         (car l)
         (last-pair2 (cdr l))
    )
)

(ds (last-pair (list 23 72 149 34)))
(ds (last-pair (list 23)))
(ds (last-pair2 (list 23 72 149 34)))
(ds (last-pair2 (list 23)))
