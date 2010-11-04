#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define t (make-hash-table))
(define (count-change_my amount)
    (define (cc amount kinds-of-coins)
        (define (evaluator a kc) 
            (if (hash-ref t (list a kc) #f) 
                (hash-ref t (list a kc))
                (begin
                    (hash-set! t (list a kc) 
                                  (+ (cc a (- kc 1))
                                     (cc (- a (first-denomination kc))
                                          kc)))
                    (hash-ref t (list a kc))
                 )
            ))
        (cond ((or (< amount 0) (= kinds-of-coins 0)) 0)
              ((= amount 0) 1)
            (else (evaluator amount kinds-of-coins))))

    (define (first-denomination kinds-of-coins)
        (cond ((= kinds-of-coins 1) 1)
            ((= kinds-of-coins 2) 5)
            ((= kinds-of-coins 3) 10)
            ((= kinds-of-coins 4) 50)
            ((= kinds-of-coins 5) 100)
            ((= kinds-of-coins 6) 200)
            ((= kinds-of-coins 7) 500)
            ((= kinds-of-coins 8) 1000)
            ((= kinds-of-coins 9) 5000)
            ((= kinds-of-coins 10) 10000)
    ))
    (cc amount 10)
)


;(define (count-change amount)
;  (cc amount 5))
;(define (cc amount kinds-of-coins)
;  (cond ((= amount 0) 1)
;        ((or (< amount 0) (= kinds-of-coins 0)) 0)
;        (else (+ (cc amount
;                     (- kinds-of-coins 1))
;                 (cc (- amount
;                         (first-denomination kinds-of-coins))
;                     kinds-of-coins)))))
;(define (first-denomination kinds-of-coins)
;  (cond ((= kinds-of-coins 1) 1)
;        ((= kinds-of-coins 2) 5)
;        ((= kinds-of-coins 3) 10)
;        ((= kinds-of-coins 4) 25)
;        ((= kinds-of-coins 5) 50)))

(define us-coins (list 25 10 5 1 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc_new amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc_new amount
                (except-first-denomination coin-values))
            (cc_new (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (no-more? l) (null? l))
(define (except-first-denomination x) (cdr x))
(define (first-denomination x) (car x))

(debug-set! stack 2000000)
(ds (cc_new 100 us-coins))
(ds (cc_new 50 uk-coins))
(d "moneu 1000: " (count-change_my 1000))
(d "moneu 11: " (count-change_my 11))
