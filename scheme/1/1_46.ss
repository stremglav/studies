#!/usr/bin/guile -s
!#
(load "utils.ss")

(define tolerance 0.00001)

(define (iterative-improve check improve)
    (define (try guess)
        (let ((next (improve guess)))
            (if (check guess next)
                next
                (try next))))
    (lambda(n) (try n)))


(define (sqrt n)
    ((iterative-improve 
        (lambda(y x) (< (abs (- y x)) tolerance)) 
        (lambda(y) (average y (/ n y))))
        n))

(define (fixed-point f n) 
    ((iterative-improve
        (lambda(y x) (< (abs (- y x)) tolerance))
        (lambda(y) (f y)))
        n)    
)

(d "Sqrt throw iterative-improve (sqrt 81)" (sqrt 81))
(d "Cos throw new fixed point (cos 1)" (fixed-point cos 1.0))

