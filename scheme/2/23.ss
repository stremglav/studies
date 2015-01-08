#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (for-each proc items)
    (proc (car items))
    (if (null? (cdr items)) 
        #t
        (for-each proc (cdr items))))

(for-each (lambda (x) (ds x))
          (list 57 321 88))

(ds (list 1 (list 2 (list 3 4))))
