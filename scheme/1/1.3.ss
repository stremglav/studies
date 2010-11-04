#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (sq a) (* a a))
(define (sq_sum a b) (+ (sq a) (sq b)))

(define (max a b c) 
    (cond ((and (> a b) (> a c)) a)
          ((and (> b a) (> b c)) b)
          (else c)))

(define (sq_maxsum a b c) 
    (max (sq_sum a b) (sq_sum a c) (sq_sum c b)))


(define (sq_maxsum2 a b c) 
    (cond ((and (< a b) (< a c)) (sq_sum b c))
          ((and (< b a) (< b c)) (sq_sum a c))
          (else (sq_sum a b))))


(d "(sq 7)" (sq 7))
(d "(sq_sum 7 4)" (sq_sum 7 4) )
(d "(max 7 4 2)" (max 7 4 2))
(d "(sq_maxsum 1 2 3)" (sq_maxsum 4 2 3))
(d "(sq_maxsum2 1 2 3)" (sq_maxsum2 1 2 3))

