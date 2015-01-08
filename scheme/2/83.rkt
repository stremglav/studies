(require racket)

; Util functions
(define (dd . args)
  (define (print l)
    (if (null? (cdr l))
        (display (car l))
        (begin
          (display (car l))
          (print (cdr l)))))
  (print args))
(define (square x) (* x x))
; end

; Mega table
(define t (make-hash))
(define (put operation type fun)
  (hash-set! t (list operation type) 
             fun))
(define (get operation type)
  (if(hash-has-key? t (list operation type))
     (hash-ref t (list operation type))
     #f))

(define tc (make-hash))
(define (put-coercion operation type fun)
  (hash-set! tc (list operation type) 
             fun))
(define (get-coercion operation type)
  (if(is-exist-c? operation type)
     (hash-ref tc (list operation type))
     #f))

(define (is-exist-c?  operation type)
  (hash-has-key? tc (list operation type)))
; end

; Auxiliary functions for table
(define (attach-tag type-tag contents)
  (if(eq? type-tag 'scheme-number)
     contents
     (cons type-tag contents)))

(define (type-tag datum)
  (if(number? datum)
     'scheme-number
     (if (pair? datum)
         (car datum) 
         (error "Некорректные помеченные данные -- TYPE-TAG" datum))))

(define (contents datum)
  (if(number? datum)
     datum
     (if (pair? datum)
         (cdr datum)
         (error "Некорректные помеченные данные -- CONTENTS" datum))))

(define (apply-generic op args)
  
  (define (is-single? l)
    (and (pair? l) (null? (cdr l))))
  
  (define (my-merge l) 
    (if (and (is-single? l)
             (pair? (car l)))
        (car l)
        l))
  
  (define (try-apply data)
    (let ((type-tags (list (type-tag (car data)))))
      (let ((proc (get op type-tags)))
        (if proc
            (proc (my-merge (map contents data)))
            #f
            ))))
  
  (define (normalize data)
    (define (check-tags l)
      (if(is-single? l)
         #t
         (foldl (lambda(x y) (and (eq? x (car l)) y)) #t l)))
    
    (define (check-coercion type data)
      (let ((res (map (lambda(x) (if(eq? type (type-tag x))
                                    x
                                    (if(is-exist-c? (type-tag x) type)
                                       ((get-coercion (type-tag x) type) x)
                                       #f))) data)))
        (if(foldl (lambda(x y) (and x y)) #t res)
           res
           #f
           )))
    
    (define (reduction data)
      (define (tmp fst scnd)
        (if(null? scnd)
           #f
           (let ((res (check-coercion (type-tag (car scnd)) data)))
             (if(pair? res)
                res
                (tmp (append fst (list (car scnd))) (cdr scnd))))))
      (tmp '() data))
    
    (if(is-single? (map type-tag data))
       data
       (reduction data)))
  
  (let ((nom-data (normalize args)))
    (if(pair? nom-data)
       (try-apply nom-data)
       (error "Нет метода для этих типов" (list op type-tags)))))
; end

; Simple scheme number package
(define (install-scheme-number-package) 
  ; interface
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number)
       (lambda (x) (tag (foldl + 0 x))))
  (put 'raise '(scheme-number)
       (lambda (x) (make-rational (car x) 1)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)
;constructor
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
; end


; Rational numbers package
(define (install-rational-package)
  ; inner procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (if (= d 0)
        (error "Знаменатель не должен быть равен 0")
        (let ((g (gcd n d)))
          (cons (/ n g) (/ d g)))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y) 
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y) 
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (eq-rat x y) 
    (and (eq (numer x) (numer y))
         (eq (denom x) (denom y))))
  (define (=zero?-rat x)
    (= (numer x) 0))
  
  ; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational)
       (lambda (x) (tag (foldl add-rat (car x) (cdr x)))))
  (put 'raise '(rational)
       (lambda (x) (begin (dd x "\n") (make-real (/ (numer x) (denom x))))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
;constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))
; end


; Real scheme number package
(define (install-scheme-real-number-package) 
  ; interface
  (define (tag x)
    (attach-tag 'real-scheme-number x))
  (put 'add '(real-scheme-number)
       (lambda (x) (tag (foldl + 0 x))))
  (put 'raise '(real-scheme-number)
       (lambda (x) (make-complex-from-real-imag (car x) 0)))
  (put 'make 'real-scheme-number
       (lambda (x) (tag (* x 1.0))))
  'done)
;constructor
(define (make-real n)
  ((get 'make 'real-scheme-number) n))
; end

; Complex numbers package
; Rectangular form of complex number
(define (install-rectangular-package)
  ; inner procedures
  (define (real-part z) (car z)) 
  (define (imag-part z) (cdr z)) 
  (define (make-from-real-imag x y) (cons x y)) 
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) 
             (square (imag-part z)))))
  (define (angle z) (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ; interface
  (define (tag x) (attach-tag 'rectangular x)) 
  (put 'real-part '(rectangular) real-part) 
  (put 'imag-part '(rectangular) imag-part) 
  (put 'magnitude '(rectangular) magnitude) 
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; Polar form of complex number
(define (install-polar-package)
  ; inner procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x)))
  ; interface
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part_my z) (apply-generic 'real-part (list z))) 
(define (imag-part_my z) (apply-generic 'imag-part (list z))) 
(define (magnitude_my z) (apply-generic 'magnitude (list z)))
(define (angle_my z)     (apply-generic 'angle (list z)))

(define (install-complex-package)
  ; процедуры, импортируемые из декартова
  ; и полярного пакетов
  (define (make-from-real-imag x y) 
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a) 
    ((get 'make-from-mag-ang 'polar) r a))
  ; inner procedures
  (define (add-complex z1 z2) 
    (make-from-real-imag (+ (real-part_my z1) (real-part_my z2))
                         (+ (imag-part_my z1) (imag-part_my z2))))
  
  ; interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex)
       (lambda (z) (tag (foldl add-complex (car z) (cdr z)))))
  
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part_my)
  (put 'imag-part '(complex) imag-part_my)
  (put 'magnitude '(complex) magnitude_my)
  (put 'angle '(complex) angle_my)
  
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (put-coercion 'scheme-number 'complex scheme-number->complex)
  'done)
; constructors
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
; end


; Generic interface
(define (add . x) (apply-generic 'add x))
(define (my_raise x) (apply-generic 'raise (list x)))
; end

; Install packages
(display "install-scheme-number-package ... ")
(install-scheme-number-package)
(display "install-rational-package ... ")
(install-rational-package)
(display "install-scheme-real-number-package ... ")
(install-scheme-real-number-package)
(display "install-rectangular-package ... ")
(install-rectangular-package)
(display "install-polar-package ... ")
(install-polar-package)
(display "install-complex-package ... ")
(install-complex-package)
