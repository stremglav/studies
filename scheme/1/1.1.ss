#!/usr/bin/guile -s
!#
(load "utils.ss")

(d "10" 10)
(d "(+ 5 3 4)" (+ 5 3 4))
(d "(- 9 1)" (- 9 1))
(d "(/ 6 2)" (/ 6 2))
(d "(+ (* 2 4) (- 4 6))" (+ (* 2 4) (- 4 6)))

(d "(define a 3)" (define a 3))
(d "(define b (+ a 1))" (define b (+ a 1)))
(d "(+ a b (* a b))" (+ a b (* a b)))
(d "(= a b)" (= a b))

(d "(if (and (> b a) (< b (*..." (if (and (> b a) (< b (* a b)))
    b
    a))

(d "(cond ((= a 4) 6)..." (cond ((= a 4) 6)
       ((= b 4) (+ 6 7 a))
       (else 25)))

(d "(+ 2 (if (> b ..." (+ 2 (if (> b a) b a)))

(d "(* (cond ((> a b) a).." (* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)))

