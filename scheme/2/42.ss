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

(define (empty-board) (list))

;(define (safe? k pos)
;    (not (foldr (lambda(x y) (or (= (car x) k)  y)) #f pos)))

(define (adjoin-position new-row k rest-of-queens)
    (cons new-row rest-of-queens))

(define (safe? k position)
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

;(define (safe? k positions)
;  (null? (filter (lambda (pos)
;                   (or (= (car pos) (caar positions))
;                       (= (abs (- (car pos) (caar positions))) (abs (- (cdr pos) (cdar positions))))))
;                 (cdr positions))))

;(ds (safe? 5 (list (cons 1 2) (cons 3 4))))

(ds (queens 4))

;(define (safe_for_all? x y l)
;(define (safe2? a b)
;    (define (diag? x1 y1 x2 y2)
;        (if (= (abs (- x1 x2)) (abs (- y1 y2))) #t #f))
;    (let ((x1 (car a))
;          (y1 (cdr a))
 ;         (x2 (car b))
 ;;         (y2 (cdr b)))
 ;        (if (or (= x1 x2) (= y1 y2) (diag? x1 y1 x2 y2)) #f #t)))
 ;   (if(null? l)
 ;       #t
  ;      (foldr (lambda(a b) (or (safe2? (cons x y) a) b)) #f l)))
;
;(ds (safe_for_all? 5 3 (list)))

;(define (create_table n) 
;    (flatmap (lambda(x) (map (lambda(y) (cons x y)) (cl 1 n))) (cl 1 n))
;)

;(ds (safe? (cons 2 5) (cons 1 2)))
;(ds (create_table 8))
;(ds (map (lambda(x) (safe? (cons 5 2) x)) (create_table 8)))

