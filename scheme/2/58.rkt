; Util functions
(define (d . args)
  (define (print l)
    (if (null? (cdr l))
        (display (car l))
        (begin
          (display (car l))
          (print (cdr l)))))
  (print args))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


; Sum
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))


; Product
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))


; Exponentiation
(define (make-exponentiation b e)
  (cond ((= e 0) 1)
        ((= e 1) b)
        (else (list b '** e))))

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '**)))

(define (base s) (car s))

(define (exponent s) (caddr s))

; Functions for creating tree from flat record
(define (create-typles l)
  (cond
    ((null? l) '())
    ((null? (cdr l)) (list (list(car l))))
    (else (cons (list (car l) (cadr l))
                (create-typles (cddr l))))))

(define (isRelated? sym l)
  (foldr (lambda(x acc)
           (and (if( = (length x) 2)
                   (not (eq? (cadr x) sym))
                   #t)
                acc))
         #t l))

(define (set-brackets l operands)
  (define (iter sym acc1 acc2)
    (cond
      ((or (isRelated? sym acc2) (= (length acc2) 1))
       (append acc1 acc2))
      ((eq? (cadr (cadr acc2)) sym)
       (iter sym acc1 (append 
                       (list (append 
                              (list (append (cadr acc2)
                                            (list (car (car acc2))))) 
                              (if(= (length (car acc2)) 1) 
                                 (list)
                                 (list (cadr (car acc2))))))
                       (cddr acc2))))
      (else (iter sym (append acc1 (list (car acc2))) (cdr acc2)))))
  (car (car (foldl (lambda(x acc) (iter x '() acc)) 
                   (reverse (create-typles l)) 
                   operands))))

(define (isCorrect-node? l)
  (= (length l) 3))


; Main function
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((not (isCorrect-node? exp)) (deriv (set-brackets exp '(** * +)) var))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp) 
                                     (make-exponentiation (base exp) 
                                                          (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "неизвестный тип выражения -- DERIV" exp))))
