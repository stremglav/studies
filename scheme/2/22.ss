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

(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (append answer
                      (list (square (car things)))))))
  (iter items nil))


(d "square-list1 (list 1 2 3 4)" (square-list1 (list 1 2 3 4)))
(d "square-list2 (list 1 2 3 4)" (square-list2 (list 1 2 3 4)))
(d "square-list3 (list 1 2 3 4)" (square-list3 (list 1 2 3 4)))

(d "cons 1 2" (cons 1 2))
(d "cons 1 (list 2)" (cons 1 (list 2)))
(d "cons (list 1) 2" (cons (list 1) 2))

