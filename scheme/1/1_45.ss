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

(define (cube_r x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (quadra_r x)
  (fixed-point ( average-damp(average-damp (lambda (y) (/ x (cube y)))))
               1.0))

(define (penta_r x)
  (fixed-point ( average-damp (average-damp(average-damp (lambda (y) (/ x (* y y y y))))))
               1.0))


(define (npow_r x n)
    (define (fun y) (/ x (npow y (1- n))))
    (define damp-count (floor (log2 n)))
    (fixed-point ((repeated average-damp damp-count) fun)
                 1.0))

(define (t_npow_r4) (npow_r 9 2))
(define (t_npow_r8) (npow_r 8 3))
(define (t_npow_r16) (npow_r 81 2))
(define (t_npow_r32) (npow_r 81 4))

(time_test t_npow_r4)
(time_test t_npow_r8)
(time_test t_npow_r16)
(time_test t_npow_r32)

