;--- Utils ---
(define (cl start end)
  (define (cl2 st acc) 
    (if (< st start)
        acc
        (cl2 (- st 1) (cons st acc))))
  (cl2 end (list )))

(define (d . args)
  (define (print l)
    (if (null? (cdr l))
        (display (car l))
        (begin
          (display (car l))
          (print (cdr l)))))
  (print args))

(define nil (list))

(define (flatmap proc seq)
  (foldr append nil (map proc seq)))
;--- end ---

(define empty-board (list))

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define (safe? pos)
  (define (check2figures? a b)
    (define (diag? x1 y1 x2 y2)
      (= (abs (- x1 x2)) (abs (- y1 y2))))
    (let ((x1 (car a))
          (y1 (cdr a))
          (x2 (car b))
          (y2 (cdr b)))
      (not (or (= x1 x2) (= y1 y2) (diag? x1 y1 x2 y2)))))
  (if(null? (cdr pos))
     #t
     (foldr (lambda(x y)
              ( and (check2figures? x (car pos)) y))
            #t (cdr pos))))

(define (queens board-size)
  (define dimension (cl 1 board-size))
  (define (queen-cols k acc)
      (if (= k 0)
        (list empty-board)
      (filter
         (lambda (positions) (begin
                               (safe? positions)))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 dimension))
          (queen-cols (- k 1) acc)))))
    (queen-cols board-size 0))

(define (queens-bad board-size)
  (define dimension (cl 1 board-size))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (begin
                               (safe? positions)))
         (flatmap (lambda (new-row)
                    (map (lambda (rest-of-queens)
                           (adjoin-position new-row k rest-of-queens))
                         (queen-cols (- k 1))))
                  dimension))))
  (queen-cols board-size))

