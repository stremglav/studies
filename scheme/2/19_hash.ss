#!/usr/bin/guile -s
!#
(load "../utils.ss")

(define us-coins (list 25 10 5 1 50))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define prod (list 160020 192035 96045 192050 32033 1122822 768020 45729 32066 80045 144050 125780 80090 304099 222998 240098 28666 32068 1152030 512014 1216020 832023 1920025 2304035 2720018 2800022 2720011 320024 336040 576065 688078 1536090 3040099 1920043 3232055 640034 480022 672024 448067 352078 320099 128045 352050 240098 16055 688070 704080 720099 752099 592058 1088026 1152030 1120041 1184034 544079 320016 384096 336043 1120032 1296056 1248094 1824065 1536099 576066 1312043 1152061 320068 320037 320090 8018 144080 80078 128092 192053 16066 8559 8574 16096 16086 3268 80064 14685 70461 32058 102463 102450 32066 32028 32006 192065 64069 64038 57627 32049 32033 48001 38425 57611 400097 400065 160041 19258 96088 256086 160086 480042 32070 19271 163204 176038 38471 32004 120021 137110 22463 28860 32004 96022 16059 38436 112014 144074 112022 112048 112085 80051 96015 112064 22490 22494 48057 48001 96015 128001 96034 112064 64093 112055 64037 64025 32050 32018 32052 32021 32010 64008 9697 25615 96036 57659 64011 727394 581874 727305 727317 581851 4821 48007 48067 48096 80025 192026 12862 16092 38467 19268 32062 224030 160054 320015 208010 96079 96008 96094 192089 144084 240015 144099 208050 160015 96051 352031 48048 48088 48053 48062 48006 48020 192035 160065 213329 53369 106790 160015 160092 145406 145456 320040 342920 342959 206500 361335 361328 206585 464574 464567 774289 373388 980686 64009 64064 64094 64044 64061 64035 64080 64011 64099 64085 64093 64065 64080 64001 64064 64005 64049 64024 64093 64017 64028 64027 64073 64092 64071 64031 64002 64061 64006 64048 64050 112012 605768 720010 416011 576076 514007 400093 533383 514001 320036 320014 775701 112097 64086 64006 320069 416089 96036 96041 128061 144035 224063 176014 224029 32001 38466 150499 144063 32003 32028 6475 144011 54446 25675 192001 9622 64015 32012 32094 32031 160073 160057 32026 25691 240026 44851 64087 64067 80052 64012 32048 32062 32067 32027 32089 32061 44888 32087 32077 32055 64025 64003 64085 64079 32002 32029 32025 32082 6444 192066 6429 6404 6438 6407 6419 19241 19251 19229 25604 22429 22459 22403 256078 208009 208055 208027 208072 224099 192067 320028 48004 32004 240013 240091 224044 224018 224005 224017 240044 240039 224069 160052 256050 160042 224095 208031 208003 208019 224025 240096 256092 224008 240006 208041 208068 208089 272045 176043 208005 457131 400084 320023 160048 160091 256077 240092 224094 224088 208095 224059 224098 224091 224001 224069 112002 160054 202122 192011 128098 208003 192084 192050 192023 192090 192017 192080 208038 320061 192099 192053 5929 5913 5957 21317))
(define counts (list 15 18 9 18 3 108 74 4 3 8 14 12 8 29 21 23 3 3 110 49 116 79 183 219 259 266 259 31 32 55 66 146 289 183 307 61 46 64 43 34 30 12 34 23 1 66 67 68 72 56 104 110 107 113 52 31 37 32 107 123 119 173 146 55 125 110 30 30 30 1 14 8 12 19 2 1 1 1 1 0 8 1 6 3 10 10 3 3 3 19 6 6 5 3 3 5 4 6 38 38 15 2 9 25 15 46 3 2 16 17 4 3 12 13 2 3 3 8 2 4 10 13 10 11 10 8 8 10 2 2 5 5 8 11 8 11 6 10 6 5 3 3 3 3 3 6 1 2 10 5 6 69 55 69 69 55 0 5 5 5 8 19 1 1 4 2 3 21 15 31 20 9 9 9 19 14 23 14 20 15 9 34 5 4 4 4 5 4 18 15 20 5 10 15 15 14 14 31 33 33 20 35 35 20 44 44 74 36 93 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 11 58 68 40 55 49 37 51 49 31 31 74 11 6 6 31 40 9 9 12 13 21 17 21 3 4 14 12 3 3 1 14 5 3 19 1 6 3 3 3 15 15 3 3 23 3 6 6 8 6 3 3 3 3 3 3 3 3 3 3 6 6 6 6 3 3 3 3 1 19 1 1 1 1 1 2 2 2 2 2 2 2 24 20 20 20 20 21 17 30 5 3 23 23 21 21 21 21 23 23 21 15 25 15 21 20 20 20 21 22 25 21 23 20 20 20 26 17 20 44 38 31 15 15 25 22 21 21 20 21 21 21 21 21 11 15 20 19 12 20 19 19 19 19 19 19 20 31 19 19 0 0 0 0))

(define (cc_simple amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc_simple amount
                (except-first-denomination coin-values))
            (cc_simple (- amount
                   (first-denomination coin-values))
                coin-values)))))


(define t (make-hash-table))
(define (cc_hash amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
            (if (hash-ref t (list amount coin-values) #f) 
                (hash-ref t (list amount coin-values))
                (begin
                    (hash-set! t (list amount coin-values)
                         (+ (cc_hash amount
                                (except-first-denomination coin-values))
                            (cc_hash (- amount
                                   (first-denomination coin-values))
                                coin-values)))
                    (hash-ref t (list amount coin-values)))
            ))))


(define (cc_show2 amount coin-values acc)
  (cond ((= amount 0) acc)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (begin (hash-set! tt (cc_show amount 
                           (except-first-denomination coin-values)
                           acc) 0)
                (hash-set! tt (cc_show (- amount (first-denomination coin-values))
                           coin-values
                           (cons (first-denomination coin-values) acc)) 0)))))

(define (overflow elem count l)
    (if(null? l)
        #f
        (if(> (foldr (lambda(cur acc)
                    (if(= cur elem) (+ 1 acc) acc)) 0 l) count)
            #f
            #t)))
(define ttt (begin (make-hash-table)))
(map (lambda(a b)(hash-set! ttt a b)) prod counts)
(define tt (make-hash-table))

(define (cc_show3 amount coin-values acc)
  (cond ((= amount 0)
            (begin (d "acc: " acc)))
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
             (begin 
                (if (overflow (first-denomination coin-values)
                              (hash-ref ttt (first-denomination coin-values))
                              acc)
                    0
                    (cc_show3 amount 
                               (except-first-denomination coin-values)
                               acc))
                (cc_show3 (- amount (first-denomination coin-values))
                          coin-values
                          (cons (first-denomination coin-values) acc))))))


(define tt (make-hash-table))
(define (cc_show amount coin-values acc)
  (cond ((= amount 0)
            (begin (d "acc: " acc)))
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
            (begin
                (if (hash-ref tt (list amount (except-first-denomination coin-values)) #f)
                    #t
                    (hash-set! tt
                               (list amount (except-first-denomination coin-values))
                               (cc_show amount 
                                        (except-first-denomination coin-values)
                                        acc)))
                (if (hash-ref tt (list (- amount (first-denomination coin-values))
                                       coin-values) #f)
                    #t
                    (hash-set! tt
                               (list (- amount (first-denomination coin-values))
                                       coin-values)
                               (cc_show (- amount (first-denomination coin-values))
                                        coin-values
                                        (cons (first-denomination coin-values) acc))))))))


(define (no-more? l) (null? l))
(define (except-first-denomination x) (cdr x))
(define (first-denomination x) (car x))


(ds (overflow 1 4 (list)))



(define (ddd k . v) 
    (begin 
    (display k)
    (display ": ")
    (display (car v))
    (display "\n")
    ))

(debug-set! stack 2000000)
;(ds (cc_simple 100 us-coins))
;(ds (cc_hash 1000 uk-coins))

(ds (cc_show3 432230 prod '()))
;(hash-for-each ddd ttt)
