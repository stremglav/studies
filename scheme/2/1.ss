#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (make-rat n d)
    (let ((g (gcd (abs n) (abs d))))
         (cons (if (< (* n d) 0)
                    (* (abs (/ n g)) -1)
                    (abs (/ n g)))
                (abs(/ d g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(dr (mul-rat (make-rat -6 -9) (make-rat 1 3)))
(dr (mul-rat (make-rat 6 -9) (make-rat -1 3)))
(dr (mul-rat (make-rat -6 -9) (make-rat -1 3)))
(dr (make-rat -6 9))
(dr (make-rat 6 -9))
(dr (make-rat -6 -9))
