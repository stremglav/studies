#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (next x) 
    (if(= x 2) 
        3
        (+ x 2)))

(define (smallest-divisor n next-fun)
  (find-divisor n 2 next-fun))
(define (find-divisor n test-divisor next-fun)
  (define (divides? a b)
        (= (remainder b a) 0))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-fun test-divisor) next-fun))))

(define (prime-simple? n)
  (= n (smallest-divisor n 1+)))
(define (prime-fast? n)
  (= n (smallest-divisor n next)))

(time_test_b "prime-fast 10000000061" (lambda() (prime-fast? 10000000061)))
(time_test_b "prime-simple 10000000061" (lambda() (prime-simple? 10000000061)))

(time_test_b "prime-fast 100000000003" (lambda() (prime-fast? 100000000003)))
(time_test_b "prime-simple 100000000003" (lambda() (prime-simple? 100000000003)))

(time_test_b "prime-fast 1000000000039" (lambda() (prime-fast? 1000000000039)))
(time_test_b "prime-simple 1000000000039" (lambda() (prime-simple? 1000000000039)))

(time_test_b "prime-fast 10000000000037" (lambda() (prime-fast? 10000000000037)))
(time_test_b "prime-simple 10000000000037" (lambda() (prime-simple? 10000000000037)))
