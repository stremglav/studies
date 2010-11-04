#!/usr/bin/guile -s
!#

(load "utils.ss")

(define (f_rec n)
    (if (< n 3)
        n
        (+ (f_rec (- n 1))
           (f_rec (- n 2)) 
           (f_rec (- n 3)))))

(define (for n)
    (if (> n 0)
        (begin
            (d "f_rec" (f_rec n))
            (for (- n 1)))))

(for 10)

(define (f_iter n)
    (iter 1 2 3 1 n))

(define (iter a b c i n)
    (if (= i n)
        (if (< i 4)
            i
            (+ a b c))
        (if (< i 4)
            (iter a b c (+ i 1) n)
            (iter b c (+ a b c) (+ i 1) n))))

(define (for2 n)
    (if (> n 0)
        (begin
            (d "f_iter" (f_iter n))
            (for2 (- n 1)))))

(for2 10)

(d "f_iter 30" (f_iter 30))
(d "f_rec 30" (f_rec 30))
