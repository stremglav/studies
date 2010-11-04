#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (make-frame1 origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame1 f) (car f))
(define (edge1-frame1 f) (cadr f))
(define (edge2-frame1 f) (caddr f))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame2 f) (car f))
(define (edge1-frame2 f) (cadr f))
(define (edge2-frame2 f) (cddr f))

(define (make-vect x y) (cons x y))
(define (xcor-vect v) (car v))
(define (ycor-vect v) (cdr v))

(define v1 (make-vect 1 2))
(define v2 (make-vect 3 2))
(define v0 (make-vect 0 0))
(define fr1 (make-frame1 v0 v1 v2))
(define fr2 (make-frame2 v0 v1 v2))

(d "make-frame1" (make-frame1 v0 v1 v2))
(d "make-frame2" (make-frame2 v0 v1 v2))
(d "origin-frame1" (origin-frame1 fr1))
(d "edge1-frame1" (edge1-frame1 fr1))
(d "edge2-frame1" (edge2-frame1 fr1))
(d "origin-frame2" (origin-frame2 fr2))
(d "edge1-frame2" (edge1-frame2 fr2))
(d "edge2-frame2" (edge2-frame2 fr2))
