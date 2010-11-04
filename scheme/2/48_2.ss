#!/usr/bin/guile -s
!#
(load "../utils.ss")


(define (Y F)
  ((lambda (x)
    (F (lambda args (apply (x x) args))))
   (lambda (x)
    (F (lambda args (apply (x x) args))))))

(ds (Y (lambda(x) 3)))
(define (ddd x) (+ 2 x))

(ds ((lambda(x) (ddd ddd)) 7))
