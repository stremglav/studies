#!/usr/bin/guile -s
!#
(load "utils.ss")

(define dx 0.01)
(define tolerance 0.00001)

(define (log2 x)
  (/ (log x) (log 2)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (compose f g)
    (lambda(x) (f (g x))))

(define (repeated f n)
    (if (= n 1)
        f
        (compose (repeated f (- n 1)) f)))

(define (npow x n)
    (if(= n 0)
        1
        (* x (npow x (1- n)))
    )
)

(define (pow_r x power count)
  (fixed-point ((repeated average-damp count) (lambda (y) (/ x (npow y (1- power)))))
               1.0))

(define (npow_r x n)
    (define (fun y) (/ x (npow y (1- n))))
    (define damp-count (floor (log2 n)))
        (fixed-point 
            ((repeated average-damp damp-count) fun)
                 1.0))

(d "pow_r 4 2 1"     (pow_r 4 2 1))
(d "pow_r 8 3 1"     (pow_r 8 3 1))
(d "pow_r 16 4 2"   (pow_r 16 4 2))
(d "pow_r 32 5 2"   (pow_r 32 5 2))
(d "pow_r 64 6 2"   (pow_r 64 6 2))
(d "pow_r 128 7 2" (pow_r 128 7 2))
(d "pow_r 256 8 2" (pow_r 256 8 3))

(d "Result:\n  npow_r 1024 10" (npow_r 1024 10))
