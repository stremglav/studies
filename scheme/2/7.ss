#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (make-interval a b) (cons a b))

(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y) 
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division by interval that spans zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (R i)
    (/ (- (upper-bound i) (lower-bound i))
       2))

(define i1 (make-interval 4 3))
(define i2 (make-interval 6 5))
(define i3 (make-interval 8 7))
(define i4 (make-interval 10 9))

(d "R (3 4)" (R i1))
(d "R (5 6)" (R i2))
(d "R (7 8)" (R i3))
(d "R (9 10)" (R i4))

(d "R (mul (3 4) (5 6))" (R (mul-interval i1 i2)))
(d "R (div (3 4) (5 6))" (R (div-interval i1 i2)))

(d "R (mul (7 8) (9 10))" (R (mul-interval i3 i4)))
(d "R (div (7 8) (9 10))" (R (div-interval i3 i4)))
