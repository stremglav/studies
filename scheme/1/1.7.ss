#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (sq x) (* x x))
(define (improve guess x)
    (average guess (/ x guess)))
(define (average x y)
    (/ (+ x y) 2))

(define (sqrt-iter guess x)
    (define (good-enough? guess x)
        (< (abs (- (sq guess) x)) 0.001))
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (sqrt-iter-new guess guess-old x)
    (define (good-enough-new? g-last g-old)
        (< (/ (* (abs (- g-last g-old)) 100) 
                 g-last) 
           0.01))
    (if (good-enough-new? guess guess-old)
        guess
        (sqrt-iter-new (improve guess x) 
                        guess
                        x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt-new x)
  (sqrt-iter-new 1.0 0.0 x))

(d "sqrt default 81" (sqrt 81))
(d "sqrt new 81" (sqrt-new 81))
(d "sqrt new 1e37" (sqrt-new 1e37))
(d "sqrt new 0.000001" (sqrt-new 0.000001))
(d "sqrt default 0.000001" (sqrt 0.000001))
(d "sqrt default 1e37" (sqrt 1e37))
