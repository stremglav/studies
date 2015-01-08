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
   (tmp n -1))

(define (car2 x)
    (iter 2 x))

(define (cdr2 x)
    (iter 3 x))

(d "cons 2 3" (cons2 2 3))
(d "car 108" (car2 108))
(d "cdr 108" (cdr2 108))

(d "car (cons 2 3)" (car2 (cons2 2 3)))
(d "cdr (cons 2 3)" (cdr2 (cons2 2 3)))
