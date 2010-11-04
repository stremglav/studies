#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (foldr op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (foldr op initial (cdr sequence)))))

(define (flatmap proc seq)
  (foldr append nil (map proc seq)))

(define empty-board (list))

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define (safe_my? a b)
    (define (diag? x1 y1 x2 y2)
        (if (= (abs (- x1 x2)) (abs (- y1 y2))) #t #f))
            (let ((x1 (car a))
                  (y1 (cdr a))
                  (x2 (car b))
                  (y2 (cdr b)))
                 (if (or (= x1 x2) (= y1 y2) (diag? x1 y1 x2 y2)) #f #t)))

(define (filter_my2 fun a l)
    (cond ((null? l) #t)
          ((fun (car l) a) (filter_my2 fun a (cdr l)))
          (else #f)))

(define (filter_my fun l)
    (cond ((null? l) #t)
          ((fun l) (filter_my fun (cdr l)))
          (else #f)))

(define (safe? k pos)
    (filter_my (lambda(l) (filter_my2 safe_my? (car l) (cdr l))) pos)
)

(define (safe2? k positions)
  (null? (filter (lambda (pos)
                   (or (= (car pos) (caar positions))
                       (= (abs (- (car pos) (caar positions))) (abs (- (cdr pos) (cdar positions))))))
                 (cdr positions))))

(define (safe3? k position)
  (define (queens-safe? queen-count rest-rows)
    (define (queen-safe? col row)
      (let ((last-col k) (last-row (car position)))
        (and (not (= last-row row))
             (not (= (abs (- last-row row))
                     (abs (- last-col col)))))))
    (cond ((null? rest-rows) true)
          ((queen-safe? queen-count (car rest-rows))
             (queens-safe? (- queen-count 1) (cdr rest-rows)))
          (else false)))
  (queens-safe? (- k 1) (cdr position)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (cl 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(d "queens 4" (queens 4))
(d "length queens 8" (length (queens 8)))
