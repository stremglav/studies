#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))

(define (make-segment a b) (cons a b))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (midpoint-segment a)
    (let ((x1 (x-point (start-segment a)))
          (y1 (y-point (start-segment a)))
          (x2 (x-point (end-segment a)))
          (y2 (y-point (end-segment a))))
        (make-point (average x1 x2) (average y1 y2))))

(dp (midpoint-segment (make-segment 
                            (make-point -2 3) 
                            (make-point 6 -5))))
