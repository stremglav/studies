#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (cube x) (* x x x))
(define (p couple) 
    (define x (car couple))
    (define i (cdr couple))
    (cons (- (* 3 x) (* 4 (cube x))) (1+ i)))

(define (sine couple)
    (define angle (car couple))
    (define index (cdr couple))
    (if (not (> (abs angle) 0.1))
       (cons angle index)
       (p (sine (cons (/ angle 3.0) index)))))

(d "sine 12.15" (sine (cons 12.15 0)))
