#!/usr/bin/guile -s
!#
(load "utils.ss")

(define tolerance 0.00001)
(define dx 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(define (cubic2 a b c)
  (lambda (x) (+ (* x (+ (* x (+ x a)) b)) c)))

(define (t_cubic) (newtons-method (cubic 0 0 -8) 1))
(define (t_cubic2) (newtons-method (cubic2 0 0 -8) 1))

(time_test t_cubic)
(time_test t_cubic2)
