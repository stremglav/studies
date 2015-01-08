(define (d . args)
  (define (print l)
    (if (null? (cdr l))
        (display (car l))
        (begin
          (display (car l))
          (print (cdr l)))))
  (print args))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set) (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree index)
  (define (tmp tree)
    (begin (d "ind: " index " tree: " tree "\n")
           (set! index (+ index 1))
           (if (null? tree)
               '() 
               (append (tmp (left-branch tree))
                       (cons (entry tree) (tmp (right-branch tree)))))))
  (tmp tree))

(define (tree->list-2 tree index)
  (define (copy-to-list tree result-list)
    (begin (d "ind: " index " tree: " tree "\n")
           (set! index (+ index 1))
           (if (null? tree) 
               result-list
               (copy-to-list (left-branch tree) 
                             (cons (entry tree)
                                   (copy-to-list (right-branch tree)
                                                 result-list))))))
  (copy-to-list tree '()))

(define tree1 '(7 
                (3 
                 (1 () ()) 
                 (5 () ())) 
                (9 () (11 () ()))))
(define tree2 '(3 
                (1 () ()) 
                (7 
                 (5 () ()) 
                 (9 () (11 () ())))))
(define tree3 '(5 
                (3 (1 () ())
                   ()) 
                (9 (7 () ()) 
                   (11 () ()))))