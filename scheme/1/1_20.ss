#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(d "НОД" (gcd 206 40))
