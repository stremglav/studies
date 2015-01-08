#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (cont-frac_r n d k)
    (define (iter a)     
        (if(= a k)
            1
        (/ (n a) (+ (d a) (iter (1+ a))))))
     (iter 1))

(define (cont-frac_i n d k)
    (define (iter a acc)     
        (if(= a 0)
            acc
            (iter (1- a) (/ (n a) (+ (d a) acc)))
        )
    )
     (iter k 0))


(define (fi_i k)
    (/ 1 (cont-frac_i (lambda (i) 1.0)
                      (lambda (i) 1.0)
                      k)))

(define (check)
    (define (f a)
        (if (> (abs (- (fi_i a) 1.6180339)) 0.00001)
            (f (1+ a))
            a))
    (f 1))

(define (t_fi_i) (fi_i 500))

(d "1" (time t_fi_i))
(d "2" (time t_fi_i))
(d "prov" (check))
