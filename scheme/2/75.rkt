(define (make-from-real-imag x y)
  (define (dispatch op) 
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (sqr x) (sqr y))))
          ((eq? op 'angle) (atan y x))
          (else (error "Неизвестная оп. -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang x y)
  (define (dispatch op) 
    (cond ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          (else (error "Неизвестная оп. -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define real-imag-sample
  (make-from-real-imag 1 3))

(define mag-ang-sample
  (make-from-mag-ang (real-imag-sample 'magnitude)
                     (real-imag-sample 'angle)))