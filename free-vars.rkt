#lang racket

;; expressions

(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * /))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op-cons op args)
  (cons op args))

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

(define (arith/let-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith/let-expr? (op-args t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def-expr (let-def t))))
      (var? t)))

;; set

(define empty-set
  (identity null))

(define (add-to-set x s)
  (cond [(null? s) (list x)]
        [(eq? x (car s)) s]
        [else (cons (car s) (add-to-set x (cdr s)))]))

(define (merge-sets s1 s2)
  (cond [(null? s1) s2]
        [else (add-to-set (car s1) (merge-sets (cdr s1) s2))]))

(define (set-member? x s)
  (member x s))
  
(define (set->list s)
  (identity s))

;; free-variables

(define (fv e env)
  (cond [(const? e) empty-set]
        [(op? e) (args-fv (op-args e) env)]
        [(let? e) (merge-sets
                    (fv (let-def-expr (let-def e)) env)
                    (fv (let-expr e)
                        (add-to-set (let-def-var (let-def e))
                                    env)))]
        [(var? e) (if (set-member? (var-var e) env)
                      empty-set
                      (add-to-set (var-var e) empty-set))]))

(define (args-fv xs env)
  (cond [(null? xs) empty-set]
        [else (merge-sets (fv (car xs) env)
                          (args-fv (cdr xs) env))]))

(define (free-vars e)
  (set->list (fv e empty-set)))
