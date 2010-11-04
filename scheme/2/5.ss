#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (pow a n) 
    (if (= n 0)
        1
    (* a (pow a (- n 1)))))

(define (cons2 x y)
    (* (pow 2 x) (pow 3 y)))

(define (iter a n)
    (define (tmp b acc)
        (if (integer? b)
            (tmp (/ b a) (1+ acc))
            acc))
   (tmp  n -1)
)

(define (car2 x)
    (iter 2 x)
)

(define (cdr2 x)
    (iter 3 x)
)

(ds (cons2 2 3))
(ds (car2 108))
(ds (cdr2 108))

;(ds (car2 (cons2 2 3)))
;(ds (cdr2 (cons2 2 3)))
