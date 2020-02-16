#lang racket

(define (inc i)
  (+ i 1))

(define (square x)
  (* x x))

(define (identity x) x)

(define (dist x y)
  (abs (- x y)))

(define (average x y)
  (/ (+ x y) 2))

(define (cube n)
  (* n n n))

;; silnia jako procedura rekurencyjna
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

;; i jako procedura iteracyjna
(define (fact-iter n i p)
  (if (= n i)
      p
      (fact-iter n (inc i) (* p (inc i)))))

;; równoważność dwóch definicji przez indukcję, wymaga uogólnienia twierdzenia do
;; dla każdego n, jeśli (natural? n) to dla każdego i (= (fact (+ n i)) (fact-iter (+ n i) i (fact i)))

;; rekurencyjna definicja ciągu Fibonacciego: rekurencyjne wywołania mogą się rozgałęziać
(define (fib n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else    (+ (fib (- n 1)) (fib (- n 2)))]))

;; trzy przykłady sumowania ciągów liczbowych
(define (sum-ints s e)
  (if (> s e)
      0
      (+ s (sum-ints (inc s) e))))

(define (sum-cubes s e)
  (if (> s e)
      0
      (+ (cube s) (sum-cubes (inc s) e))))

(define (sum-pi n e)
  (if (> n e)
      0
      (+ (/ 1 (* n (+ n 2))) (sum-pi (+ n 4) e))))

;; ogólna procedura: abstrakcja wyższego rzędu wyrażająca sumowanie wyrazów *pewnego* szeregu
(define sum
  (lambda (term next s e)
    (if (> s e)
        0
        (+ (term s) (sum term next (next s) e)))))

;; alternatywny zapis ostatniego z szeregów: zamiast sum-pi-next używamy formy lambda — nienazwanej procedury
(define (sum-pi-alt n e)
  (define (sum-pi-term s)
    (/ 1 (* s (+ s 2))))
  (define (sum-pi-next s)
    (+ s 4))
  (sum sum-pi-term (lambda (s) (+ s 4)) n e))


(define (close-enough? x y)
  (< (dist x y) 0.0000000001))

;; obliczanie (przybliżonego) punktu stałego funkcji f przez iterację, let pozwala uniknąć powtarzania obliczeń
(define (fix-point f x0)
  (let ((x1 (f x0)))
    (if (close-enough? x0 x1)
        x1
        (fix-point f x1))))

;; próba obliczania pierwiastka kwadratowego z x jako punktu stałego funkcji y ↦ x / y zapętla się
;; stosujemy tłumienie z uśrednieniem: procedurę wyższego rzędu zwracającą procedurę jako wynik
(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (sqrt-ad x)
  (fix-point (average-damp (lambda (y) (/ x y))) 1.0))

;; obliczanie pochodnej funkcji z definicji przyjmując dx za "odpowiednio małą" wartość (zamiast "prawdziwej" granicy)
(define (deriv f)
  (let ((dx 0.000001))
    (lambda (x) (/ (- (f (+ x dx)) (f x)) dx))))

;; przekształcenie Newtona: x ↦ x - f(x) / f'(x) pozwala obliczyć miejsce zerowe f jako punkt stały tej transformacji
(define (newton-transform f)
  (lambda (x)
    (- x
       (/ (f x)
          ((deriv f) x)))))

(define (newtons-method f x)
  (fix-point (newton-transform f) x))

;; zastosowania
(define pi (newtons-method sin 3))

(define (sqrt-nm x)
  (newtons-method (lambda (y) (- x (square y))) 1.0))

;; możemy wyabstrahować wzorzec widoczny w definicjach sqrt: znaleźć punkt stały pewnej funkcji *przy użyciu* transformacji
;; argumentem fix-point-of-transform jest procedura przetwarzająca procedury w procedury!
(define (fix-point-of-transform transform f x)
  (fix-point (transform f) x))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Lista2
(define (compose f g)
  (lambda (x)
    (f(g x))))

;((compose square inc) 5)
;((compose inc square) 5)

(define (repeated p n)
  (if (= n 0)
      identity
      (compose p (repeated p (- n 1)))))

(define (product term next s e)
  (if (> s e)
      1
      (* (term s) (product term (next s) e))))

#;(define (approx-pi steps)
  (define (inc2 x) (+ x 2))
  (define (proc s) (product identity inc2 s (+ s (* 2 steps)))))

(define (accumulateI combiner null-value term s next e)
  (define (iter el acc)
    (if (> el e)
        acc
        (iter (next el) (combiner acc (term el)))))
    (iter s null-value))

(define (accumulateR combiner null-value term s next e)
  (if (> s e)
      null-value
      (combiner (term s) (accumulateR combiner null-value term (next s) next e))))

(define (cont-fracR num den k)
  (define (recur i)
    (if (= k i)
        (/ (num i) (den i))
        (/ (num i) (+ (den i) (recur(+ i 1))))))
  (recur 1.0))

(define (cont-fracI num den k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (num i) (+ (den i) result)))))
  (iter k 0.0))

(define (cont-frac num den k)
  (cont-fracI num den k))

;7num
#;(define (num i)
  (square (- (+ i i) 1)))

;8 
(define (atan-cf x k)
  (define (num x)
    (lambda (i)
      (if (= i 1)
          x
          (square(* (- i 1) x)))))
  (define (den i)
    (- (+ i i) 1))
  (cont-frac (num x) den k))

;9
(define (build n d b)
  (/ n (+ d b)))

(define (repeated-build k n d b)
  (if (= k 0)
      b
      (repeated-build (- k 1) n d (build n d b))))

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

(define (atan-cf2 x)
  (define (num x)
    (lambda (i)
      (if (= i 1)
          x
          (square(* (- i 1) x)))))
  (define (den i)
    (- (+ i i) 1))
  (calc-cont-frac (num x) den))