#!/usr/bin/guile -s
!#
(load "utils.ss")

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))



(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) tolerance))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (iterative-improve check improve)
    (define (try guess)
        (let ((next (improve guess)))
            (if (check guess next)
                next
                (try next))))
    (lambda(n) (try n)))


(define (my_sqrt n)
    ((iterative-improve 
        (lambda(y x) (< (abs (- y x)) tolerance)) 
        (lambda(y) (average y (/ n y))))
        n))

(define (new_fpoint f n) 
    ((iterative-improve
        (lambda(y x) (< (abs (- y x)) tolerance))
        (lambda(y) (f y)))
        n)    
)

(define (t_sqrt) (my_sqrt 81))
(define (t_cos) (new_fpoint cos 1.0))

(time_test t_sqrt)
(time_test t_cos)

