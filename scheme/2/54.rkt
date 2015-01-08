(define (sicp-equal? a b)
  (cond
    ((and (null? a) (null? b)) #t)
    ((and (symbol? a) (symbol? b))(eq? a b))
    ((and (number? a) (number? b))(= a b))
    ((and (pair? a) (pair? b))
     (and (sicp-equal? (car a) (car b))
          (sicp-equal? (cdr a) (cdr b))))
    (else #f)))