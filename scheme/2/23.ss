#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (for-each proc items)
  (proc (car items))
  (if (not (null? (cdr items))) (for-each proc (cdr items)) #t)
)

; (define (for-each proc items) 
;     (proc (car items)) 
;     (if (not (null? (cdr items))) 
;         (for-each proc (cdr items)) 
;         #t)) 

(for-each (lambda (x) (ds x))
          (list 57 321 88))
