#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (sq x) (* x x))
(define (abs x)
  (cond ((< x 0) (- x))
         (else x)))
(define (average x y)
  (/ (+ x y) 2))

(define (cube-iter guess guess_old x)
    (define (good-enough-proc? g_last g_old)
        (< (/ (* (abs (- g_last g_old)) 100) g_last) 0.1))
    (define (improve3 guess x)
        (/ (+ (/ x (sq guess)) (* 2 guess)) 3))
    (if (good-enough-proc? guess guess_old)
        guess
        (cube-iter (improve3 guess x)
                    guess
                    x)))

(define (cube x)
  (cube-iter 1.0 0.0 x))

(d "cube 5" (cube 5))
