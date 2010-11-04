#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (cube x) (* x x x))

(define (sum1 term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum1 term (next a) next b))))

(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))
    (iter a 0))


(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

(define (simpson f a b n) 
    (define h (/ (- b a) n))
    (define (arg k) (+ a (* k h)))
    (define (inc x) (+ x 1))
    (define (fun k) 
            (cond ((or (= k 0) (= k n)) (f (arg k)))
                  ((even? k) (* 2 (f (arg k))))
                  (else (* 4 (f (arg k))))))
    (* (sum fun 0 inc n) (/ h 3))
)

(d "integral: " (integral cube 0 1 0.01))
(d "simpson: " (simpson cube 0 1 1000))
