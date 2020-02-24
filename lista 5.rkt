#lang racket

( define (var? t)
   ( symbol? t))

(define (make-neg x)
  (list 'neg x))

(define (neg-subf x)
  (second x))

( define (neg? t)
   ( and ( list? t)
         (= 2 ( length t))
         (eq? 'neg (car t))))

(define (make-conj x y)
  (list 'conj x y))

(define (conj-left x)
  (second x))

(define (conj-rght x)
  (third x))

( define ( conj? t)
   ( and ( list? t)
         (= 3 ( length t))
         (eq? 'conj (car t))))

(define (make-disj x y)
  (list 'disj x y))

(define (disj-left x)
  (second x))

(define (disj-rght x)
  (third x))

( define ( disj? t)
   ( and ( list? t)
         (= 3 ( length t))
         (eq? 'disj (car t))))

( define ( prop? f)
   (or ( var? f)
       ( and ( neg? f)
             ( prop? ( neg-subf f)))
       ( and ( disj? f)
             ( prop? ( disj-left f))
             ( prop? ( disj-rght f)))
       ( and ( conj? f)
             ( prop? ( conj-left f))
             ( prop? ( conj-rght f)))))

(define (free-vars x)
  (define (find x vlist)
    (cond ((var? x)
           (if(not(member x vlist))
              (cons x vlist)
              vlist))
          ((neg? x)
           (find (neg-subf x) vlist))
          ((conj? x)
           (find (conj-left x) (find (conj-rght x) vlist)))
          ((disj? x)
           (find (disj-left x) (find (disj-rght x) vlist)))))
  (find x '()))

#|(define and-2or (make-conj (make-disj 'z 'y) (make-disj (make-neg 'z) 't)))
(prop? and-2or)
(free-vars and-2or)|#

( define ( gen-vals xs)
   (if ( null? xs)
       ( list null )
       ( let*
            (( vss ( gen-vals (cdr xs)))
             (x (car xs))
             (vst ( map ( lambda (vs) ( cons ( list x true ) vs)) vss ))
             (vsf ( map ( lambda (vs) ( cons ( list x false ) vs)) vss )))
          ( append vst vsf))))

#;
(define (eval-formula f v)
  (define (value var)
    (define (help val)
    (if (null? val)
        (error)
        (if(eq?(caar val) var)
           (cadar val)
           (help (cdr val)))))
    (help v))
  (cond((var? f)(value f))))

#;
(define (falsifiable-eval? f )
  (define (iter l)
    (if (null?  l)
        false
        (if(not (eval-formula f (car l)))
           (car l)
           (iter (cdr l)))))
  (iter (gen-vals(free-vars f))))

(define (concat-map f xs)
  (if (null? xs)
      null
      (append (f (car xs)) (concat-map f (cdr xs)))))


(define (sublists xs)
  (if (null? xs)
      (list null)
      (concat-map (lambda (vs) (list (cons (car xs) vs) vs)) (sublists (cdr xs)))))

(sublists '(1 2))