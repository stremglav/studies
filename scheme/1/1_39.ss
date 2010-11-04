#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (icont-frac n d k)
    (define (iter a acc)     
        (if(= a 0)
            acc
            (iter (1- a) (/ (n a) (+ (d a) acc)))
        )
    )
     (iter k 0))


(define (tan-cf x k)
  (icont-frac (lambda (i) (if (= i 1) x (- (* x x))))
             (lambda (i) (- (* 2 i) 1))
             k))

(d "tan-cf" (tan-cf 2.0 10))
