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

;; let-lifted expressions

(define (arith-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith-expr? (op-args t)))
      (var? t)))

(define (let-lifted-expr? t)
  (or (and (let? t)
           (let-lifted-expr? (let-expr t))
           (arith-expr? (let-def-expr (let-def t))))
      (arith-expr? t)))

;; generating a symbol using a counter

(define (next-var i)
  (number->symbol i))

(define (number->symbol i)
  (string->symbol (string-append "x" (number->string i))))

;; environments (could be useful for something)

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; set
;; z wykładu

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
;; z wykładu

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

;; evaluator
;; z wykładu

(define (eval-env e env)
  (cond [(const? e) e]
        [(op? e)
         (apply (op->proc (op-op e))
                (map (lambda (a) (eval-env a env))
                     (op-args e)))]
        [(let? e)
         (eval-env (let-expr e)
                   (env-for-let (let-def e) env))]
        [(var? e) (find-in-env (var-var e) env)]))

(define (env-for-let def env)
  (add-to-env
    (let-def-var def)
    (eval-env (let-def-expr def) env)
    env))

(define (eval e)
  (eval-env e empty-env))

;; the let-lift procedure

(define (let-extr e lets num env)

  (define (let-extr-op newexprlist exprlist lets num env)
    (if (null? exprlist)
        (list (reverse newexprlist) lets num)
        (let [(x (let-extr (car exprlist) lets num env))]
          (let-extr-op (cons (car x) newexprlist)
                       (cdr exprlist) (cadr x) (caddr x) env))))

  (cond [(const? e) (list e lets num)]
        [(op? e)
         (let [(x (let-extr-op '() (op-args e) lets num env))]
           (list (cons (op-op e) (car x)) (cadr x) (caddr x)))]
        [(let? e)
         (let [(x (let-extr (let-def-expr(let-def e)) lets num env))]
           (let-extr (let-expr e) (add-to-env (next-var (caddr x))
                                              (car x) (cadr x))
                     (+ (caddr x) 1)
                     (add-to-env (let-def-var(let-def e))
                                 (next-var (caddr x)) env)))]
        [(var? e) (list (find-in-env (var-var e) env) lets num)]))


(define (let-lift e)

  (define (final-expr lets expr)
    (if (null? lets)
        expr
        (final-expr (cdr lets) (let-cons (car lets) expr))))

  (define (check-free-vars e)
    (define (add-fv-to-env fvlist env)
      (if (null? fvlist)
          env
          (add-fv-to-env (cdr fvlist)
                         (add-to-env (car fvlist) (car fvlist) env))))
    (add-fv-to-env (free-vars e) empty-env))

  (cond [(not(arith/let-expr? e))
         (error "WRONG EXPR")]
        [(let-lifted-expr? e) e]
        [else
         (let ((answer (let-extr e empty-env 0
                                 (check-free-vars e))))
           (final-expr (cadr answer) (car answer)))]))

;;test

(define test-list '[
                    (+ 10 (* ( let (x 7) (+ x 2)) 2))
                    ( let (x (- 2 ( let (z 3) z))) (+ x 2))
                    (+ ( let (x 5) x) ( let (x 1) x))
                    (/ e 1)
                    (+ (let (x 5) x) x)
                    (* 3 10)
                    ( let (x 7) (+ 10 (* (+ x 2) 2)))
                    ( let (x ( let (x 4) (+ 1 x))) (let (x 3) x))
                    ]
  )

(define (test-let-lift)
  (let-lift '
            (+ 10 (* ( let (x1 7) (+ x1 2)) 2))
            ))

(define (test-eval)
  (eval(let-lift '
                 (+ 10 (* ( let (x1 7) (+ x1 2)) 2))
                 )))

;; (eval (let-lift x)=(eval x)
(define (test)
  (define (iter list1 list2)
    (let [(x (free-vars (car list1)))]
      (if (not(null? x))
          [and
           (display "Free vars:")
           (displayln x)
           (displayln (car list1))
           (if (or (null? (cdr list1)) (null? (cdr list2)))
               (display "")
               (iter (cdr list1) (cdr list2)))]
          [and
           (display (equal? (eval (car list1)) (eval(car list2))))
           (display " ")
           (displayln (eval(car list1)))
           (displayln (car list1))
           ;;(displayln (car list2))
           (if (or (null? (cdr list1)) (null? (cdr list2)))
               (display "")
               (iter (cdr list1) (cdr list2)))])))
    (iter (map let-lift test-list)
        test-list))

(test)