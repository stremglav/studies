#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (foldr op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (foldr op initial (cdr sequence)))))

(define (foldl op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(d "foldr /" (foldr / 1 (list 1 2 3)))
(d "foldl /" (foldl / 1 (list 1 2 3)))
(d "foldr list" (foldr list nil (list 1 2 3)))
(d "foldl list" (foldl list nil (list 1 2 3)))
(d "foldr x^y + y^x" (foldr (lambda(x y) (+ (expt x y) (expt y x))) 0 (list 1 2 3)))
(d "foldl x^y + y^x" (foldl (lambda(x y) (+ (expt x y) (expt y x))) 0 (list 1 2 3)))
