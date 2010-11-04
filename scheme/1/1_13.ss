#!/usr/bin/guile -s
!#
(load "utils.ss")

(define fi  (/ (+ 1 (sqrt 5)) 2))

(define (apr_fib n) 
    (/ (expt fi n) (sqrt 5)))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(show_list apr_fib)
(show_list fib)
