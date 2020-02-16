#lang racket

;predykaty pomocnicze
;;;;;;;;;;;;;;;;;;;;
(define (square x)
  (* x x))

(define (identity x) x)

(define (dist x y)
  (abs (- x y)))

(define (close-enough? x y)
  (< (dist x y) 0.0000000001))

(define (compose f g)
  (lambda (x)
    (f(g x))))

(define (repeated p n)
  (if (<= n 0)
      identity
      (compose p (repeated p (- n 1)))))

(define (fix-point f x0)
  (let ((x1 (f x0)))
    (if (close-enough? x0 x1)
        x1
        (fix-point f x1))))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))
;;;;;;;;;;;;;;;;;;;;

;procedura z pojedynczym tłumieniem
;;;;;;;;;;;;;;;;;;;;
(define (rt-ad x)
  (fix-point (average-damp (lambda (y) (/ x (expt y 1)))) 1.0))

;Założenie: wystarczy (/ n 2) tłumień
;;;;;;;;;;;;;;;;;;;;
(define (nth-root n x)
  (fix-point (repeated (average-damp (lambda (y) (/ x (expt y (- n 1))))) (* 3 n)) 1.0))

;testy
;;;;;;;;;;;;;;;;;;;;
#|
(nth-root 3 8)
(nth-root 3 27)
(nth-root 4 81)
(nth-root 4 1024)
|#