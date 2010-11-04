#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (process-tree combiner leaf null-value tree)
  (cond ((null? tree) null-value)
        ((not (pair? tree)) (leaf tree))
        (else (combiner (process-tree combiner leaf null-value (car tree))
                        (process-tree combiner leaf null-value (cdr tree))))))

(define (pick-leaves predicate tree)
  (define (xcar x)
    (list 'car x))

  (define (xcdr x)
    (list 'cdr x))

  (define (combine l f)
    (map (lambda (a) (lambda (x) (a (f x)))) l))

  (process-tree (lambda (l r) (append (combine l xcar) (combine r xcdr)))
                (lambda (a) (if (predicate a) (list (lambda (x) x)) '()))
                '()
                tree))

(define (pick n tree)
  (map (lambda (a) (a tree))
       (pick-leaves (lambda (x) (= x n)) tree)))

(ds (pick 7 '(1 3 (5 7) 9)))
(ds (pick 7 '((7))))
(ds (pick 7 '(1 (2 (3 (4 (5 (6 7))))))))
