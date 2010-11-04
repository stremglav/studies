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

(define (reverser sequence)
  (foldr (lambda (x y) (append y (list x)) ) nil sequence))
(define (reversel sequence)
  (foldl (lambda (x y) (cons y x) ) nil sequence))

(ds (foldr append nil '((1 2 3) (2 4) (3 6))))
(d "reverser" (reverser '(1 2 3 4 5 6)))
(d "reversel" (reversel '(1 2 3 4 5 6)))
