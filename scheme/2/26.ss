#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (reverse l)
    (if (null? l)
        l
        (append (reverse (cdr l))  (list (car l)))))

(define (deep-reverse l)
    (cond ((null? (cdr l)) l)
          ((not (pair?  (car l))) l)
          (else (cons (deep-reverse (cdr l))  (deep-reverse (car l))))
          
    )
)

;(ds (reverse (list 23 (list 72 149) 99 34)))
(ds (deep-reverse (list (list 33 23) (list 72 149) (list 99 34))))
