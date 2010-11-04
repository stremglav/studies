#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (sq x) (* x x))

(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
    )
)

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (sq guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(d "new_if: " (new-if (= 2 3) 0 5))
(d "sqrt default 81: " (sqrt 81))
