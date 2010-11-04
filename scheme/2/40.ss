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

(define (flatmap proc seq)
  (foldr append nil (map proc seq)))

(define (unique-pairs n)
    (flatmap (lambda(i) 
                (map (lambda (j) (list i j))
                             (cl 1 (- i 1)))) 
             (cl 1 n)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
    (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(d "unique-pairs" (unique-pairs 4))
(d "prime-sum-pairs" (prime-sum-pairs 4))
