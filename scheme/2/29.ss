#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (branch-length branch) (car branch))
(define (branch-structure branch) (cadr branch))


(define isWeight? pair?)

(define (total-weight tree)
    (cond ((not (pair? tree)) 0)
          ((not (isWeight? (branch-structure tree))) (branch-structure tree))
          (else (+ (total-weight (left-branch tree))
                   (total-weight (right-branch tree))))))

(define (isBalanced? mobile)
    (define (branch-weight branch)
        (let ((item (branch-structure branch)))
            (if (isWeight? item)
                (total-weight item)
                item)))
    (define (isBranch-balanced? branch)
        (if (isWeight? (branch-structure branch))
            (isBalanced? (branch-structure branch))
            true))
    (define (moment branch)
        (* (branch-length branch) (branch-weight branch)))
    (let ((l (left-branch mobile))
         (r (right-branch mobile)))
    (and (= (moment l) (moment r))
         (isBranch-balanced? l)
         (isBranch-balanced? r))))


(define branch1 (make-branch 18 (make-mobile (make-branch 3 8) (make-branch 2 12))))
(define branch2 (make-branch 20 (make-mobile (make-branch 5 8) (make-branch 4 10))))

(define tree0 (make-mobile (make-branch 3 4) (make-branch 4 3)))
(define tree1 (make-mobile branch1 branch2))
(define tree2 (make-mobile (make-branch 1 (make-mobile (make-branch 1 3) (make-branch 5 7)))
                           (make-branch 4 5)))

(d "isBalanced? tree0" (isBalanced? tree0))
(d "isBalanced? tree1" (isBalanced? tree1))
(d "isBalanced? tree2" (isBalanced? tree2))
(d "total-weight tree0" (total-weight tree0))
(d "total-weight tree1" (total-weight tree1))
(d "total-weight tree2" (total-weight tree2))

(define (make-mobile2 left right)
  (cons left right))
(define (make-branch2 length structure)
  (cons length structure))
(define (left-branch2 tree) (car tree))
(define (right-branch2 tree) (cdr tree))
(define (branch-length2 branch) (car branch))
(define (branch-structure2 branch) (cdr branch))

(define tree02 (make-mobile2 (make-branch2 3 4) (make-branch2 4 3)))

(d "\nright-branch2" (right-branch2 tree02))
(d "branch-structure2"  (branch-structure2 (right-branch2 tree02)))
