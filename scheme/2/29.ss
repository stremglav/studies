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

(define (total-weight tree)
    (cond ((not (pair? tree)) 0)
          ((not (pair? (branch-structure tree))) (branch-structure tree))
          (else (+ (total-weight (left-branch tree))
                   (total-weight (right-branch tree))))))



(define (rot_inst br)
    (* (total-weight br) 
       (branch-length br)))

(define (balanced? tree)
    (= (rot_inst (left-branch tree))
       (rot_inst (right-branch tree))
    )
)

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (pair? struct)
        (total-weight struct)
        struct)))

(define (balanced2? mobile)
  (define (branch-balanced? branch)
    (if (pair? (branch-structure branch))
        (balanced? (branch-structure branch))
        true))
  (define (torque branch)
    (* (branch-length branch) (branch-weight branch)))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (branch-balanced? left)
         (branch-balanced? right)
         (= (torque left) (torque right)))))


(define (branch-balanced? branch) 
   (let ((s (branch-structure branch))) 
     (if (structure-is-mobile? s) 
         (balanced? s) 
         true))) 
  
 (define (branch-torque branch) 
   (* (branch-weight branch) 
      (branch-length branch)))

 (define (balanced3? mobile) 
  
   (let ((left (left-branch mobile)) 
         (right (right-branch mobile))) 
     (and (= (branch-torque left) 
             (branch-torque right)) 
          (branch-balanced? left) 
          (branch-balanced? right)))) 



(define bb (make-branch 3 5))
(define branch1 (make-branch 4 (make-mobile (make-branch 3 8) (make-branch 6 9))))
(define branch1_1 (make-branch 4 (make-mobile (make-branch 3 8) (make-branch 6 4))))
(define branch1_1 (make-branch 4 (make-mobile (make-branch 3 8) (make-branch 6 4))))
(define branch2 (make-branch 1 (make-mobile (make-branch 5 3) (make-branch 6 9))))
(define tree0 (make-mobile (make-branch 3 4) (make-branch 4 3)))
(define tree1 (make-mobile branch1 branch2))
(define tree2 (make-mobile (make-branch 1 (make-mobile (make-branch 1 3) (make-branch 5 7))) (make-branch 4 5)))


(define level-1-mobile (make-mobile (make-branch 2 1) 
                                    (make-branch 1 2))) 
(define level-2-mobile (make-mobile (make-branch 3 level-1-mobile) 
                                    (make-branch 9 1))) 
(define level-3-mobile (make-mobile (make-branch 4 level-2-mobile) 
                                    (make-branch 8 2)))

(d "level-1-mobile" (balanced? level-1-mobile))
(d "level-2-mobile" (balanced? level-2-mobile))
(d "level-3-mobile" (balanced? level-3-mobile))

(d "addishen" (balanced? (make-mobile (make-branch 10 1000) 
                                      (make-branch 1 level-3-mobile))))

(d "addishen2" (balanced2? (make-mobile (make-branch 10 1000) 
                                      (make-branch 1 level-3-mobile))))

(d "addishen3" (balanced3? (make-mobile (make-branch 10 1000) 
                                      (make-branch 1 level-3-mobile))))
(d "t_w level-3-mobile" (total-weight level-3-mobile))

(d "total_my 1" (total-weight tree0))
(d "total_my 2" (total-weight tree1))
(d "total_my 2" (total-weight tree2))
