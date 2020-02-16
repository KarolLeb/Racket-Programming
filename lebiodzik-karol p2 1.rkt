#lang racket

;predykaty dodatkowe
;;;;;;;;;;;;;;;;;;;;
(define (square x)
  (* x x))

(define (dist x y)
  (abs (- x y)))

;wartość przybliżenia
(define (close-enough? x y)
  (< (dist x y) 0.00000000001))

(define (cont-fracI num den k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (num i) (+ (den i) result)))))
  (iter k 0.0))
;;;;;;;;;;;;;;;;;;;;

;zad8/lista2
;obliczanie arctg
;;;;;;;;;;;;;;;;;;;;
(define (atan-cf x k)
  (define (num x)
    (lambda (i)
      (if (= i 1)
          x
          (square(* (- i 1) x)))))
  (define (den i)
    (- (+ i i) 1))
  (cont-fracI (num x) den k))
;;;;;;;;;;;;;;;;;;;;

;obliczanie ułamków łańcuchowych z warunkiem stopu w close-enough?
;;;;;;;;;;;;;;;;;;;;
(define (calc-cont-frac num den)
  (define A1 (num 1.0))
  (define B1 (den 1.0))
  (define A2 (* (den 2.0) A1))
  (define B2 (+ (* (den 2.0) B1) (num 2.0)))
  (define (iter i prevA actA prevB actB)
    (define (calc-next X-1 X-2)
      (+ (* (den i) X-1) (* (num i) X-2)))
    (let ((nextA (calc-next actA prevA))
          (nextB (calc-next actB prevB)))
      (let ((Fk+1 (/ nextA nextB))
            (Fk (/ actA actB)))
        (if (close-enough? Fk Fk+1)
            Fk+1
            (iter (+ i 1) actA nextA actB nextB)))))
  (iter 3.0 A1 A2 B1 B2))
;;;;;;;;;;;;;;;;;;;;

;nowy sposób na arctg, wykorzystujący nowy predykat
;;;;;;;;;;;;;;;;;;;;
(define (atan-cf2 x)
  (define (num x)
    (lambda (i)
      (if (= i 1)
          x
          (square(* (- i 1) x)))))
  (define (den i)
    (- (+ i i) 1))
  (calc-cont-frac (num x) den))
;;;;;;;;;;;;;;;;;;;;

;testy
;;;;;;;;;;;;;;;;;;;;
(define pi 3.141592653589793)
(atan pi)
(atan-cf2 pi)
;(atan-cf pi 100)
(atan 5)
(atan-cf2 5)
;(atan-cf 5 100)
(atan 1)
(atan-cf2 1)
;(atan-cf 1 100)