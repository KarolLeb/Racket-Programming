#lang racket

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (hole? t)
  (eq? t 'hole))

(define (arith/let/holes? t)
  (displayln t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t)
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))

(define (hole-context e)
  ;; DONE
  (define (iter e context)
    (cond [(hole? e) context]
          [(var? e) (var-cons e)]
          [(binop? e)
           (if (= 1 (num-of-holes (binop-left e)))
               (iter (binop-left  e) context)
               (iter (binop-right e) context))]
          [(let? e)
           (if (= 1 (num-of-holes (let-def-expr (let-def e))))
               (iter (let-def-expr (let-def e)) context)
               (iter (let-expr e) (cons (let-def-var (let-def e)) context)))]))
  (if (arith/let/hole-expr? e)
      (remove-duplicates (iter e '()))
      (error "WRONG EXPRESSION!")))

;;(hole-context '(let (y (let (x 7) 1)) y))
(define test-list '
    (+ (+ (let (y 7) 9) (let (z 10) hole)) (let (x 5) 7))
    #;(let (y (let (x 7) hole)) y)
    #;((+ 3 hole )
                       (let (x 3) (let (y 7) (+ x hole)))
                       (let (x 3) (let (y hole) (+ x 3)))
                       (let (x hole) (let (y 7) (+ x 3)))
                       (let (piesek 1)
                         (let (kotek 7)
                           (let (piesek 9)
                              (let (chomik 5)
                                 hole))))
                       (+ ( let (x 4) 5) hole ))
    )

(define (test)
  ;; TODO:
  #;(define test-list '
    (+ (+ (let (y 7) 9) (let (z 10) hole)) (let (x 5) 7))
    (let (y (let (x 7) hole)) y)
    ((+ 3 hole )
                       (let (x 3) (let (y 7) (+ x hole)))
                       (let (x 3) (let (y hole) (+ x 3)))
                       (let (x hole) (let (y 7) (+ x 3)))
                       (let (piesek 1)
                         (let (kotek 7)
                           (let (piesek 9)
                              (let (chomik 5)
                                 hole))))
                       (+ ( let (x 4) 5) hole )))
  (hole-context test-list)
  )

;;procedura testowa:   (do usunięcia)??
(define (test2)
  #;(define test-list '
    (+ (+ (let (y 7) 9) (let (z 10) hole)) (let (x 5) 7))
    (let (y (let (x 7) hole)) y)
    ((+ 3 hole )
                       (let (x 3) (let (y 7) (+ x hole)))
                       (let (x 3) (let (y hole) (+ x 3)))
                       (let (x hole) (let (y 7) (+ x 3)))
                       (let (piesek 1)
                         (let (kotek 7)
                           (let (piesek 9)
                              (let (chomik 5)
                                 hole))))
                       (+ ( let (x 4) 5) hole ))
      )
  (arith/let/holes? test-list))