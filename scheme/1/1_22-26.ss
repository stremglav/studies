#!/usr/bin/guile -s
!#
(load "utils.ss")

(define (runtime)
    (define (tmp t)
        (+ (* (car t) 1000) (round (/ (cdr t) 1000))))
    (tmp (gettimeofday))
)

(define (next x) 
    (if(= x 2) 
        3
        (+ x 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (min_p_after n) 
    (define (tmp n acc) 
        (if (= (length acc) 3)
            acc
            (if (fast-prime? n 100)
              (tmp (+ n 1) (append acc (list n)))
              (tmp (+ n 1) acc))))
    (tmp n (list)))

(define (expmod1 base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod1 base (/ exp 2) m))
                     m))
        (else
         (remainder (* base (expmod1 base (- exp 1) m))
                     m))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (expmod2 base exp m)
  (remainder (fast-expt base exp) m))


(define (fermat-test n)
  (define (try-it a)
    (= (expmod1 a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))


(time_test_f "3 first prime number after 10000000000" (lambda() (min_p_after 10000000000)))
(time_test_f "3 first prime number after 10000000000" (lambda() (min_p_after 100000000000)))
(time_test_f "3 first prime number after 10000000000" (lambda() (min_p_after 1000000000000)))
(time_test_f "3 first prime number after 10000000000" (lambda() (min_p_after 10000000000000)))
