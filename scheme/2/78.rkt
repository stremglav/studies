(require racket)

; Util functions
(define (d . args)
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
  (hash-ref t (list operation type)))
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

(define (apply-generic op . args)
  (begin
    (d "(apply-generic " op " . " args ")\n")
    (d "type-tags: " (map type-tag args) "\n")
    (d "contents: " (map contents args) "\n")
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (error
             "Нет метода для этих типов -- APPLY-GENERIC"
             (list op type-tags)))))))
; end

; Simple scheme number package
(define (install-scheme-number-package) 
  ; interface
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y)))) 
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y)))) 
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y)))) 
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y)))) 
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
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
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
  
  ; interface
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
;constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))
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

(define (real-part_my z) (apply-generic 'real-part z)) 
(define (imag-part_my z) (apply-generic 'imag-part z)) 
(define (magnitude_my z) (apply-generic 'magnitude z))
(define (angle_my z) (apply-generic 'angle z))

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
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part_my z1) (real-part_my z2))
                         (- (imag-part_my z1) (imag-part_my z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude_my z1) (magnitude_my z2))
                       (+ (angle_my z1) (angle_my z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude_my z1) (magnitude_my z2))
                       (- (angle_my z1) (angle_my z2))))
  
  ; interface
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) real-part_my)
  (put 'imag-part '(complex) imag-part_my)
  (put 'magnitude '(complex) magnitude_my)
  (put 'angle '(complex) angle_my)
  'done)

; constructors
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
; end

; Generic interface
(define (add x y) (apply-generic 'add x y)) 
(define (sub x y) (apply-generic 'sub x y)) 
(define (mul x y) (apply-generic 'mul x y)) 
(define (div x y) (apply-generic 'div x y))
; end

; Install packages
(display "install-scheme-number-package ... ")
(install-scheme-number-package)
(display "install-rational-package ... ")
(install-rational-package)
(display "install-rectangular-package ... ")
(install-rectangular-package)
(display "install-polar-package ... ")
(install-polar-package)
(display "install-complex-package ... ")
(install-complex-package)
