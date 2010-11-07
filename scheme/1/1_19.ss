#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (square x) (* x x))

(define (fib1 n)
  (fib-iter1 1 0 n))
(define (fib-iter1 a b count)
  (if (= count 0)
      b
      (fib-iter1 (+ a b) a (- count 1))))


(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))  ; вычислить p’
                   (+ (* 2 p q) (square q)); вычислить q’
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(d "fib_log"  (map fib (cl 1 10) ))
(d "fib_simple"  (map fib1 (cl 1 10) ))

(time_test_b "fibonacci 100000 log time" (lambda() (fib 100000)))
(time_test_b "fibonacci 100000 simple time" (lambda() (fib1 100000)))
