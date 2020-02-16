#lang racket


(define (dist x y)
  (abs (- x y)))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (cube-root x)
  (define (improve approx)
    (/ (+ (/ x (square approx))(* 2 approx)) 3))

  (define (good-enough? approx)
    (< (dist x (cube approx)) 0.000001))

  (define (iter approx)
    (cond
      [(good-enough? approx) approx]
      [else                  (iter (improve approx))]))
  
   (iter 1.0))

;(cube-root 9261000)
;210.00000000000009
;(cube-root 1000000000000000000000)
;10000000.0
;(cube-root -75937550)
;-423.4663059382724
;(cube-root 191248044)
;191248044.00000003
;(cube-root -615898894)
;-850.8176188129582