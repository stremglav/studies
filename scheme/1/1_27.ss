#!/usr/bin/guile -s
!#
(load "utils.ss")

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
        (cond ((= a 1) #t)
              ((= (expmod a n n) a) (try-it (- a 1)))
              (else #f)))
    (try-it (- n 1)))

(d "fermat-test" (map fermat-test (list 561 1105 1729 2465 2821 6601)))
