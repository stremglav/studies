#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves x)
    (accumulate (lambda(f s) 
                   (+ (cond ((null? f) 0)
                            ((not (pair? f)) 1)
                            (else (count-leaves f)))
                      s))
                0 x))

(define tree1 (list 1 (list 2 (list 3 4))))
(define tree2 (list (list (list 1 2 3 4 5 6 7) 8 (list) ) (list 9 (list 10 11))))
(define tree3 (list (list (list 1 2 3 4 5 6 7) 8 (list 1) ) (list 9 (list 10 11))))

(d "count-leaves tree1" (count-leaves tree1))
(d "count-leaves tree2" (count-leaves tree2))
(d "count-leaves tree3" (count-leaves tree3))
