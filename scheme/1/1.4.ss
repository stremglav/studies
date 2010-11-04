#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))


(d "a-plus-abs-b 1 -3" (a-plus-abs-b 1 -3))
(d "a-plus-abs-b 1 3" (a-plus-abs-b 1 3))

