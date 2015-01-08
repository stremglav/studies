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


(define wave (gen-wave "white"))
(define wave1 (gen-wave "green"))
(define wave2 (gen-wave "red"))

(define (sicp-below a b) (above (scale/xy 1 1/2 b) (scale/xy 1 1/2 a)))
(define (sicp-beside a b) (beside (scale/xy 1/2 1 a) (scale/xy 1/2 1 b)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (sicp-beside painter (sicp-below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (sicp-below painter (sicp-beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (sicp-beside up up))
              (bottom-right (sicp-below right right))
              (corner (corner-split painter (- n 1))))
          (sicp-beside (sicp-below painter top-left)
                       (sicp-below bottom-right corner))))))