#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (next x) 
    (if(= x 2) 
        3
        (+ x 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (min_p_after n) 
    (define (tmp n acc) 
        (if (= (length acc) 3)
            acc
            (if (prime? n )
              (tmp (+ n 1) (append acc (list n)))
              (tmp (+ n 1) acc))))
    (tmp n (list)))

(time_test_f "3 first prime number after 10000000000" (lambda() (min_p_after 10000000000)))
(time_test_f "3 first prime number after 10000000000" (lambda() (min_p_after 100000000000)))
(time_test_f "3 first prime number after 10000000000" (lambda() (min_p_after 1000000000000)))
(time_test_f "3 first prime number after 10000000000" (lambda() (min_p_after 10000000000000)))
