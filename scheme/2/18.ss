#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (reverse l)
    (if (null? l)
        (list)
        (append (reverse (cdr l))  (list (car l)))
    )
)

(define (reverse2 items)
  (define (reverse-iter source result)
    (if (null? source)
        result
        (reverse-iter (cdr source) (cons (car source) result))))
  (reverse-iter items (list)))

(ds (reverse (list 23 72 149 34)))
(ds (reverse (list 23)))

(ds (reverse2 (list 23 72 149 34)))
(ds (reverse2 (list 23)))
