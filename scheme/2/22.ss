#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (square-list1 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list2 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (list (square (car things)))))))
  (iter items nil))


(ds (square-list1 (list 1 2 3 4)))
(ds (square-list2 (list 1 2 3 4)))

(ds (cons (list 1 2 3) 3))
(ds (cons 3 (list 1 2 3)))

