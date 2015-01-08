(require 2htdp/image)

(define (gen-wave color)
  (add-curve 
   (add-curve (rectangle 100 100 "solid" "black")
              30 20 0 0.4
              70 80 0 0.4
              color)
   48 45 0 -0.3
   30 80 0 -0.3
   color))

(define wave (gen-wave "green"))

(define (sicp-below a b) (above (scale/xy 1 1/2 b) (scale/xy 1 1/2 a)))
(define (sicp-beside a b) (beside (scale/xy 1/2 1 a) (scale/xy 1/2 1 b)))

(define (split fun1 fun2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (fun1 painter (fun2 smaller smaller)))))
  (lambda(painter n) (iter painter n)))

(define right-split (split sicp-beside sicp-below))
(define up-split (split sicp-below sicp-beside))