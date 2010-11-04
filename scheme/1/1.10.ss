#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(d "A 1 10: " (A 1 10))
(d "A 2 4: " (A 2 4))
(d "A 3 3: " (A 3 3))
