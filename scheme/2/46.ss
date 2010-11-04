#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define (oper v1 v2 op)
    (cons (op (xcor-vect v1) (xcor-vect v2))
          (op (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
    (oper v1 v2 -))
(define (add-vect v1 v2)
    (oper v1 v2 +))
(define (scale-vect s v) 
    (cons (* s (xcor-vect v))
          (* s (ycor-vect v))))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 2))

(d "sub v1 v2" (sub-vect v1 v2))
(d "add v1 v2" (add-vect v1 v2))
(d "scale 3 v1" (scale-vect 3 v1))
