#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (cr-seg-list l)
    (map make-segment l (append (cdr l) (list (car l)))))

(define wave1 (segments->painter (cr-seg-list '(0.1 0.2 0.3 0.5))))
(define wave2 (segments->painter (cr-seg-list '(0.1 0.2 0.6 0.3 0.5))))

(define (corner-split1 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))
(define (corner-split2 painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner)))))

(define (square-limit1 painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))
(define (square-limit2 painter n)
  (let ((combine4 (rotate90 (square-of-four flip-horiz identity
                                  rotate180 flip-vert))))
    (combine4 (corner-split painter n))))
