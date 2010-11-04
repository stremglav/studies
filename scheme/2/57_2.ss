#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (foldr op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (foldr op initial (cdr sequence)))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (make-sum-list seq)
  (if (null? (cdr seq))
      (car seq)
      (cons '+ seq)))

(define (augend s)
  (make-sum-list (cddr s)))

(define (make-sum . seq)
  (let ((vars (filter non-number? seq))
        (const (foldr + 0 (filter number? seq))))
    (cond ((null? vars) const)
          ((= 0 const) (make-sum-list vars))
          (else (make-sum-list (cons const vars))))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (make-product-list seq)
  (if (null? (cdr seq))
      (car seq)
      (cons '* seq)))

(define (multiplicand p)
  (make-product-list (cddr p)))

(define non-number? (lambda(x) (not (number? x))))


(define (make-product . seq)
  (let ((vars (filter non-number? seq))
        (const (foldr * 1 (filter number? seq))))
    (cond ((null? vars) const)
          ((= 0 const) 0)
          ((= 1 const) (make-product-list vars))
          (else (make-product-list (cons const vars))))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))

(define (make-exponentiation b e)
    (cond ((= e 0) 1)
          ((= e 1) b)
          (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp) 
                                     (make-exponentiation (base exp) 
                                                          (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))

(ds (deriv '(* x y (+ x 3)) 'x))
(ds (deriv '(* x y (+ x 3) (+ x 4)) 'x))
