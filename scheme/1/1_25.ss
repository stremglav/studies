#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (expmod1 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod1 base (/ exp 2) m))
                     m))
        (else
         (remainder (* base (expmod1 base (- exp 1) m))
                     m))))

(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-prime-tmpl? n times expmod)
    (define (fermat-test n)
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(define (liza-prime? n times)
    (fast-prime-tmpl? n times expmod2))

(define (fast-prime? n times)
    (fast-prime-tmpl? n times expmod1))

(ds (expmod1 2 5 3))
(ds (expmod2 2 5 3))
(ds (remainder 1 3))
;(time_test_b "fast-prime 10000000061" (lambda() (fast-prime? 10000000061 1000)))
;(time_test_b "liza-prime 10000000061" (lambda() (liza-prime? 10000000061 1000)))

;(time_test_b "fast-prime 100000000003" (lambda() (fast-prime? 100000000003 1000)))
;(time_test_b "liza-prime 100000000003" (lambda() (liza-prime? 100000000003 1000)))

;(time_test_b "fast-prime 1000000000039" (lambda() (fast-prime? 1000000000039 1000)))
;(time_test_b "liza-prime 1000000000039" (lambda() (liza-prime? 1000000000039 1000)))

;(time_test_b "fast-prime 10000000000037" (lambda() (fast-prime? 10000000000037 1000)))
;(time_test_b "liza-prime 10000000000037" (lambda() (liza-prime? 10000000000037 1000)))
