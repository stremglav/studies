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


(define (e k)
    (+ 2 (icont-frac (lambda (i) 1.0)
                     (lambda (i) (if (= (remainder i 3) 2)
                                     (* 2 (/ (1+ i) 3))
                                     1))
                      k)))

(define (eiler k)
    (if (= (remainder k 3) 1)
          (+ (* 2 (/ (1- k) 3)) 2)
          1))

(d "e" (e 100))
(d "e" (e 10))
