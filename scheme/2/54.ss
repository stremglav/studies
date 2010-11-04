#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (equal2? a b)
  (cond ((and (null? a) (null? b)) true)
        ((and (number? a) (number? b)) (eq? a b))
        ((or (null? a) (null? b)) false)
        ((and (not (pair? (car a))) (not (pair? (car b)))) (if(eq? (car a) (car b)) (equal2? (cdr a) (cdr b)) false))
        ((and (pair? (car a)) (pair? (car b))) (and (equal2? (car a) (car b)) (equal2? (cdr a) (cdr b))))
        (else false)))

(define (equal3? a b)
  (if (and (pair? a) (pair? b))
      (and (equal3? (car a) (car b)) (equal3? (cdr a) (cdr b)))
      (eq? a b)))

(d "equal2? 1 1" (equal2? 1 1))
(d "equal2? 1 2" (equal2? 1 2))
(d "equal2? '(1) '(1)" (equal2? '(1) '(1)))
(d "equal2? '(2) '(1)" (equal2? '(2) '(1)))
(d "equal2? '(2 3 4) '(2 3 )" (equal2? '(2 3 4) '(2 3 )))
(d "equal2? '(2 (3 3) 4) '(2 (3 3) 4)" (equal2? '(2 (3 3) 4) '(2 (3 3) 4)))
(d "equal2? '(2 (3 3) 4) '(2 (3) 4)" (equal2? '(2 (3 3) 4) '(2 (3) 4)))
(d "equal2? '(2 (3 3) 4) '(2 (3 3) 5)" (equal2? '(2 (3 3) 4) '(2 (3 3) 5)))
(d "equal2? '(2 (3 (1 2 3) 3) 5) '(2 (3 (1 2 3) 3) 5)" (equal2? '(2 (3 (1 2 3) 3) 5) '(2 (3 (1 2 3) 3) 5)))
(d "equal2? '(2 (3 (1 3) 3) 5) '(2 (3 (1 2 3) 3) 5)" (equal2? '(2 (3 (1 3) 3) 5) '(2 (3 (1 2 3) 3) 5)))
