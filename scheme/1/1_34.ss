#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (f g)
  (g 2))


(f 2)
(f f)
