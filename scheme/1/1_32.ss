#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (accumulate_r combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate_r combiner null-value term (next a) next b))))

(define (accumulate_i combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))

(define (sum term a next b)
    (accumulate_r + 0 term a next b))

(define (product term a next b)
    (accumulate_i * 1 term a next b))

(define (fac n)
    (define (inc i) (+ i 1))
    (define (elem x) x)
    (product elem 1 inc n))

(define (summ n)
    (define (inc i) (+ i 1))
    (define (elem x) x)
    (sum elem 1 inc n))

(define (t_fac) (fac 10))
(define (t_summ) (summ 10))

(time_test t_fac)
(time_test t_summ)
