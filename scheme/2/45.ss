(define (split fun1 fun2)
  (define (iter painter n)
    (if (= n 0)
        painter
        (let ((smaller (iter painter (- n 1))))
          (fun1 painter (fun2 smaller smaller)))))
  generic-split)
