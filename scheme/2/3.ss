#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (make-point x y) (cons x y))
(define (x-point x) (car x))
(define (y-point x) (cdr x))

(define (make-segment a b) (cons a b))
(define (start-segment x) (car x))
(define (end-segment x) (cdr x))

(define (rect a b) (cons a b))
(define (rect2 a h w) 
    (cons a 
          (cons (+ (x-point a) h) 
                (+ (y-point a) w))))

(define (side_a rect)
     (abs (- (x-point (start-segment rect))
             (x-point (end-segment rect)))))

(define (side_b rect)
    (abs (- (y-point (start-segment rect))
            (y-point (end-segment rect)))))

(define (sq rect) 
    (* (side_a rect) (side_b rect)))

(define (per rect)
    (+ ( * 2 (side_a rect)) (* 2 (side_b rect))))

(define a (make-point -2 3))
(define b (make-point -3 -2))

(d "sq" (sq (rect a b)))
(d "per" (per (rect a b)))

(d "sq" (sq (rect2 b 1 5)))
(d "per" (per (rect2 b 1 5)))
