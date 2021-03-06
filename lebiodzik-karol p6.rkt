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
  ;;(displayln t)
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

(define test-list '(
                    (+ (+ (let (y 7) (let (z 10) hole)) 9) (let (x 5) 7))
                    (let (y (let (x 7) hole)) 0)
                    (+ (- 1 5) (/ x hole))
                    (let (m 2) (let (y 4) (let (m 6) hole))) 
                    )
  )

(define test-res '(
                   (y z)
                   (x)
                   ()
                   (m y)
                   )
  )

(define (test)
  (define (iter list1 list2)
    (display (equal? (car list1) (car list2)))
    (display " ")
    (displayln (car list1))
    (if (or (null? (cdr list1)) (null? (cdr list2)))
        (display "")
        (iter (cdr list1) (cdr list2))))
  (iter (map (lambda (x) (sort (hole-context x) symbol<?)) test-list)
        test-res))

(define (test2)
  (map arith/let/holes? test-list))

(define (test3)
  (map (lambda (x) (sort (hole-context x) symbol<?)) test-list))

(test)