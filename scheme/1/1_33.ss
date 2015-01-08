#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (filtered-accumulate_r combiner null-value term a next b pred)
    (if (> a b)
        null-value
        (if(pred a)
            (combiner (term a)
                      (filtered-accumulate_r combiner null-value term (next a) next b pred))
            (filtered-accumulate_r combiner null-value term (next a) next b pred))))

(define (sum term a next b pred)
    (filtered-accumulate_r + 0 term a next b pred))

(define (sqrmod x m)
  (let ((y (remainder (square x) m)))
    (if (and (= y 1) (not (= x 1)) (not (= x (- m 1))))
        0
        y)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (sqrmod (expmod base (/ exp 2) m) m))
        (else (remainder (round (* base (expmod base (- exp 1) m))) m))))

(define (witness? a n)
  (not (= (expmod a (- n 1) n) 1)))

(define (miller-rabin-test n)
  (not (witness? (+ 1 (random (- n 1))) n)))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (prime? x) (fast-prime? x (- x 1)))

(define (sum_sq_simple1 a b)
    (define (inc x) (+ x 1))
    (filtered-accumulate_r + 0 square a inc b prime?))

(define (sum_sq_simple a b)
    (sum square a 1+ b prime?))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (mega-sum n)
    (define (pred x) (= (gcd x n) 1))
    (define (id x) x)
    (filtered-accumulate_r * 1 id 1 1+ n pred ))

(define (t_sum) (sum_sq_simple 1 10))
(define (t_sum1) (sum_sq_simple1 1 10))
(define (t_gcd) (gcd 206 40))
(define (t_mega-sum) (mega-sum 10))

(time_test t_sum)
(time_test t_sum1)
(time_test t_gcd)
(time_test t_mega-sum)
