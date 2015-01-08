#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (simple-prime? n)
    (define (smallest-divisor n next-fun)
      (find-divisor n 2 next-fun))
    (define (find-divisor n test-divisor next-fun)
      (define (divides? a b)
            (= (remainder b a) 0))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next-fun test-divisor) next-fun))))
  (= n (smallest-divisor n 1+)))

(define (fast-prime? n times)
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
            ((even? exp)
             (remainder (square (expmod base (/ exp 2) m))
                         m))
            (else
             (remainder (* base (expmod base (- exp 1) m))
                         m))))
    (define (fermat-test n)
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(time_test_b "fast-prime 10000000061" (lambda() (fast-prime? 10000000061 1000)))
(time_test_b "simple-prime 10000000061" (lambda() (simple-prime? 10000000061)))

(time_test_b "fast-prime 100000000003" (lambda() (fast-prime? 100000000003 1000)))
(time_test_b "simple-prime 100000000003" (lambda() (simple-prime? 100000000003)))

(time_test_b "fast-prime 1000000000039" (lambda() (fast-prime? 1000000000039 1000)))
(time_test_b "simple-prime 1000000000039" (lambda() (simple-prime? 1000000000039)))

(time_test_b "fast-prime 10000000000037" (lambda() (fast-prime? 10000000000037 1000)))
(time_test_b "simple-prime 10000000000037" (lambda() (simple-prime? 10000000000037)))
