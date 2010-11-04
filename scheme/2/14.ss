#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (make-interval a b) (cons a b))

(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y) 
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division by interval that spans zero" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c per)
    (define r (/ (* per c) 100))
    (make-interval (- c r) (+ c r)))

(define (percent i)
    (define (oper fun) (fun (lower-bound i) (upper-bound i)))
    (* 100 (/ (oper -) (oper +))))


(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))


(d "1 - center" (center (par1 (make-center-percent 500 0.1) 
                              (make-center-percent 650 0.2))))

(d "2 - center" (center (par2 (make-center-percent 500 0.1) 
                              (make-center-percent 650 0.2))))

(d "1 - per" (percent (par1 (make-center-percent 500 0.1) 
                              (make-center-percent 650 0.2))))

(d "2 - per" (percent (par2 (make-center-percent 500 0.1) 
                              (make-center-percent 650 0.2))))

(d "div" (percent (div-interval (make-center-percent 500 0.1) (make-center-percent 500 0.1))))
