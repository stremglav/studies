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
                 (cl 1 k)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(define (queens2 board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
        (flatmap
         (lambda (new-row)
           (map (lambda (rest-of-queens)
                  (adjoin-position new-row k rest-of-queens))
                (queen-cols (- k 1))))
         (cl 1 board-size))))

)
  (queen-cols board-size))

(d "queens old" (queens 5))
(d "length queens old" (length (queens 5)))

(d "queens new" (queens2 5))
(d "length queens new" (length (queens2 5)))
