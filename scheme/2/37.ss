#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda(x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons (list) mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda(x) (matrix-*-vector cols x)) m)))


(define test '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
(define A '((2 3) (5 7)))
(define B '((-1 2) (-2 3)))
(define v (list 1 2 3))
(define w (list 4 5 6))
(d "dot-product" (dot-product v w))
(d "matrix-*-vector" (matrix-*-vector test v))
(d "transpose" (transpose test))
(d "matrix-*-matrix" (matrix-*-matrix A B))
