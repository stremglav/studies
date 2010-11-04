#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (rcont-frac n d k)
    (define (iter a)     
        (if(= a k)
            1
        (/ (n a) (+ (d a) (iter (1+ a))))))
     (iter 1))

(define (icont-frac n d k)
    (define (iter a acc)     
        (if(= a 0)
            acc
            (iter (1- a) (/ (n a) (+ (d a) acc)))
        )
    )
     (iter k 0))


(define (fi k)
    (/ 1 (icont-frac (lambda (i) 1.0)
                     (lambda (i) 1.0)
                      k)))

(define (check)
    (define (f a)
        (if (> (abs (- (fi a) 1.6180339)) 0.00001)
            (f (1+ a))
            a))
    (f 1))

(d "prov" (check))
