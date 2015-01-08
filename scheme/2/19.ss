#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define test (list  2 5 1))
(define us-coins (list 25 10 1 5 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc_new amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc_new amount
                (except-first-denomination coin-values))
            (cc_new (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? l) (null? l))
(define (except-first-denomination x) (cdr x))
(define (first-denomination x) (car x))


(d "cc 100 us-couns" (cc_new 100 us-coins))
(d "cc 50 uk-coins" (cc_new 50 uk-coins))
(d "cc 10 test" (cc_new 10 test))
