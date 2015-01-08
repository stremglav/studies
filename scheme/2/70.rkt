(define (make-leaf symbol weight) 
  (list 'leaf symbol weight))

(define (leaf? object) 
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right) 
  (list left
        right 
        (append (symbols left) (symbols right)) 
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree) 
  (if (leaf? tree)
      (list (symbol-leaf tree)) 
      (caddr tree)))

(define (weight tree) 
  (if (leaf? tree)
      (weight-leaf tree) 
      (cadddr tree)))

(define (decode bits tree) 
  (define (decode-1 bits current-branch)
    (if (null? bits) 
        '()
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch) 
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree)) 
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch) 
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "плохой бит -- CHOOSE-BRANCH" bit))))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol s tree)
  (define (inList? s set)
    (cond ((null? set) #f)
          ((eq? (car set) s) #t)
          (else (inList? s (cdr set)))))
  (if(null? tree)
     '()
     (let ((l (left-branch tree))
           (r (right-branch tree)))
       (cond ((not (inList? s (symbols tree)))
              (error "плохой символ -" s))
             ((and (leaf? l) (inList? s (symbols l))) '(0))
             ((and (leaf? r) (inList? s (symbols r))) '(1))
             ((inList? s (symbols l))
              (cons 0 (encode-symbol s l)))
             ((inList? s (symbols r))
              (cons 1 (encode-symbol s r)))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '() 
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if(null? set)
     set
     (if (null? (cdr set))
         (car set)
         (successive-merge 
          (adjoin-set 
           (make-code-tree (car set) (cadr set))
           (cddr set))))))
  
  
  (define sample-tree 
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree (make-leaf 'B 2)
                                    (make-code-tree (make-leaf 'D 1)
                                                    (make-leaf 'C 1)))))
  
  (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
  
  (define song '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip sha boom))
  
  (define song-alphabet '((na 16) (yip 9) (sha 3) (a 2) (get 2) (job 2) (boom 1) (wah 1)))
  (define song-tree (generate-huffman-tree song-alphabet))
  
  (define fixed-encoded-length (* 3 (length song)))
  (define huffman-encoded-length (length (encode song song-tree)))