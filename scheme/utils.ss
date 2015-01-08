;***
; Pretty print
;***
(define (d x y) (format #t "~a : ~s\n" x y))

(define (dg x y) (format #t "~a ~s\n" x y))

(define (ds y) (format #t "~s\n" y))

(define (dr x)
    (format #t "~s/~s\n" (numer x) (denom x)))

(define (dp x)
    (format #t "(~s,~s)\n" (car x) (cdr x)))

(define (show_list fun)
    (map (lambda (i) (d i (fun i))) (cl 0 9)))

;***
; Cross interpreter
;***
(define nil '())

(define true #t)

(define false #f)

;***
; Math
;***
(define (square x) (* x x))

(define (cube x) (* x x x))

(define (average a b) (/ (+ a b) 2.0))

;***
; Timer functions
;***
(define (runtime)
    (define (tmp t)
         (+ (* (car t) 1000000) (cdr t)))
     (tmp (gettimeofday)))

(define (priv_time_test fun writer)
    (define (start st)
        (let ((res (fun))
              (t (- (runtime) st)))
             (writer res t)))
    (start (runtime)))


(define (time fun)
    (define (start st)
        (fun)
        (- (runtime) st))
    (start (runtime)))

(define (time_test fun)
    (define (writer r t)
        (if(> t 1000 )
            (format #t "~a: ~s ms\n" r (round (/ t 1000)))
            (format #t "~a: ~s mks\n" r t)))
    (priv_time_test fun writer))

(define (time_test_g x fun)
    (define (writer r t) (format #t "~a ~s\n" x t))
    (priv_time_test fun writer))

(define (time_test_t fun)
    (define (writer r t)
        (if(> t 1000 )
            (format #t "~s ms\n" (round (/ t 1000)))
            (format #t "~s mks\n" t)))
    (priv_time_test fun writer))

(define (time_test_b sig fun)
    (define (writer r t)
        (if(> t 1000 )
            (format #t "~a: ~s ms\n" sig (round (/ t 1000)))
            (format #t "~a: ~s mks\n" sig t)))
    (priv_time_test fun writer))

(define (time_test_f sig fun)
    (define (writer r t)
        (if(> t 1000 )
            (format #t "~a:\n ~a --- ~s ms\n" sig r (round (/ t 1000)))
            (format #t "~a:\n ~a --- ~s mks\n" sig r t)))
    (priv_time_test fun writer))

(define (graph n init inc fun)
    (define (group n num fun)
         (round (/ (foldr + 0
                          (cdr
                             (map (lambda(x)
                                     (gc)
                                     (time (lambda() (fun x))))
                                  (cln n num))))
                   n)))
     (dg init (group 5 init fun))
     (if (> n 1)
         (graph (1- n) (+ init inc) inc fun)))


;***
; List functions
;***
(define (cl start end)
    (define (cl2 st acc) 
    (if (< st start)
        acc
        (cl2 (- st 1) (cons st acc))))
    (cl2 end (list )))

(define (cln n num)
    (map (lambda(x) num) (cl 1 n))
)

(define (foldr op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (foldr op initial (cdr sequence)))))


;***
; Other functions 
;***
(define (prime? x) 
    (define (sqrmod x m)
       (let ((y (remainder (square x) m)))
            (if (and (= y 1) (not (= x 1)) (not (= x (- m 1))))
                0
                y)))
    (define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp) (sqrmod (expmod base (/ exp 2) m) m))
              (else (remainder (round (* base (expmod base (- exp 1) m))) m))))
    (define (witness? a n)
        (not (= (expmod a (- n 1) n) 1)))
    (define (miller-rabin-test n)
        (not (witness? (+ 1 (random (- n 1))) n)))
    (define (fast-prime? n times)
        (cond ((= times 0) #t)
              ((miller-rabin-test n) (fast-prime? n (- times 1)))
              (else #f)))
    (fast-prime? x (- x 1)))
