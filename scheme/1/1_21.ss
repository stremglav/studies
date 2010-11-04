#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(d "НОД" (smallest-divisor 19999))
(d "НОД" (smallest-divisor 1999))
(d "НОД" (smallest-divisor 199))
