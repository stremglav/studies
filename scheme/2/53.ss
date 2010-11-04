#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))


(ds (list 'a 'b 'c))
(ds (list (list 'george)))
(ds (cdr '((x1 x2) (y1 y2))))
(ds (cadr '((x1 x2) (y1 y2))))
(ds (pair? (car '(a short list))))
(ds (memq 'red '((red shoes) (blue socks))))
(ds (memq 'red '(red shoes blue socks)))

