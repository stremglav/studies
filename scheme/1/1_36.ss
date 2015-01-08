#!/usr/bin/guile -s
!#
(load "utils.ss")

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess i)
    (format #t "~s: ~s \n" i guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (1+ i)))))
  (try first-guess 1))


(define (equation)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
               2))


(define (equation2)
  (fixed-point (lambda (x) (/ (+ (/ (log 1000) (log x)) x) 2))
               2))

(equation)
(equation2)
