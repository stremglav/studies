#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (foldr op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (foldr op initial (cdr sequence)))))

(define (ifoldr op initial sequence)
    (define (iter l acc)
        (if (null? l)
            acc
            (iter (cdr l) (op (car l) acc))))
    (iter sequence initial))

(define (filter pred l)
    (ifoldr (lambda(x y) 
                (if (pred x)
                    (cons x y)
                    y))
            nil l))

(define (flatmap proc seq)
  (foldr append nil (map proc seq)))

(define (in_list l a)
    (cond ((null? l) #f)
          ((= (car l) a) #t)
          (else (in_list (cdr l) a))))

(define (addition a b)
    (define (iter li acc)
             (cond ((null? li) acc)
                   ((in_list b (car li)) (iter (cdr li) acc))
                   (else (iter (cdr li) (cons (car li) acc))))
    )
    (reverse (iter a nil))
)

(define (unique_triples_my n)
    (flatmap (lambda(i)
        (flatmap (lambda(j)
                (map (lambda(k) 
                        (list i j k)) 
                     (addition (cl 1 n) (list i j)))) 
             (addition (cl 1 n) (list i)))) 
        (cl 1 n)))

(define (unique_triples n)
    (flatmap (lambda (i)
          (flatmap (lambda (j)
                (map (lambda (k) (list i j k))
                     (cl 1 (- j 1))))
                   (cl 1 (- i 1))))
             (cl 1 n)))

(define (triples_with_sum s n)
  (filter (lambda (t) (= (foldr + 0 t) s))
          (unique_triples n)))

(define (triples_with_sum_my s n)
  (filter (lambda (t) (= (foldr + 0 t) s))
          (unique_triples_my n)))

(d "addition" (addition (cl 1 20) (cl 4 15)))
(d "unique_triples" (unique_triples 5))
(d "unique_triples_my" (unique_triples_my 3))
(d "triples_with_sum" (triples_with_sum 18 7))
(d "triples_with_sum_my" (triples_with_sum_my 18 7))
