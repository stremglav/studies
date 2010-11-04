#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (sq x) (* x x))

(define (sq_sum x y) (+ (sq x) (sq y)))

(define (sq_max a b c) (
    max (sq_sum a b) (sq_sum a c) (sq_sum c b)
 ))

(d "sq_max" (sq_max 4 3 2))

(define (abs x)
  (cond ((< x 0) (- x))
         (else x)))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)
    )
)

(define (sqrt-iter_new guess guess_old x)
  (if (good-enough_new? guess guess_old)
      guess
      (sqrt-iter_new (improve guess x) guess
                     x)))

(define (cube-iter guess guess_old x)
  (if (good-enough_new? guess guess_old)
      guess
      (cube-iter (improve3 guess x) guess
                     x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (improve3 guess x)
    (/ (+ (/ x (sq guess)) (* 2 guess)) 3)
)

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (sq guess) x)) 0.001))

(define (good-enough_proc? g_last g_old)
  (< (/ (* (abs (- g_last g_old)) 100) g_last) 0.1))

(define (good-enough_new? g_last g_old)
  (< (abs (- g_last g_old)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(define (sqrt_new x)
  (sqrt-iter_new 1.0 0.0 x))

(define (cube x)
  (cube-iter 1.0 0.0 x))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define t (make-hash-table))
(define (count-change amount)
    (define (cc amount kinds-of-coins)
        (define (evaluator a kc) 
            (if (hash-ref t (list a kc) #f) 
                (hash-ref t (list a kc))
                (begin
                    (hash-set! t (list a kc) 
                                  (+ (cc a (- kc 1))
                                     (cc (- a (first-denomination kc))
                                          kc)))
                    (hash-ref t (list a kc))
                 )
            ))
        (cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
              ((= amount 0) 1)
            (else (evaluator amount kinds-of-coins))))

    (define (first-denomination kinds-of-coins)
        (cond ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 50)
            ((= kinds-of-coins 5) 100)
            ((= kinds-of-coins 6) 200)
            ((= kinds-of-coins 7) 500)
            ((= kinds-of-coins 8) 1000)
            ((= kinds-of-coins 9) 5000)
            ((= kinds-of-coins 10) 10000)
    ))
    (cc amount 10)
)

(define (func n) 
    (if (< n 3)
        n
        (+ func(n - 1) func(n - 2) func(n - 3))
    ))

(d "new_if: " (new-if (= 2 3) 0 5))
(d "sqrt default 1e-20: " (sqrt 1e-20))
(d "sqrt new 1e-20:     " (sqrt_new 1e-20))
;(d "sqrt default 1e37:  " (sqrt 1e37)) уходит в бесконечность, этим доказывается некоректность абсолютной дельты
(d "A 1 10: " (A 1 10))
(d "A 2 4: " (A 2 4))
;(d "A 2 5: " (A 2 5))
(d "cube: " (cube 5))
(debug-set! stack 200000)
(d "moneu 1000: " (count-change 1000))
(d "moneu 11: " (count-change 11))

