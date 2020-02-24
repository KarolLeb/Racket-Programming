#lang racket

;; definiujemy reprezentację liczb wymiernych

(define (make-rat n d)
  (let ((c (gcd n d)))
    (cons (/ n c) (/ d c))))

(define (rat-numer l)
  (car l))

(define (rat-denum l)
  (cdr l))

(define (rat? l)
  (and (pair? l)
       (not (= (rat-denum l) 0))
       (= 1 (gcd (rat-numer l) (rat-denum l)))))


;; i pakiet operacji dla użytkownika; wykorzystujemy konstruktory *nie patrząc* na to jak są zaimplementowane

(define (int->rat n)
  (make-rat n 1))

(define (div-rat l1 l2)
  (let ((n (* (rat-numer l1) (rat-denum l2)))
        (d (* (rat-denum l1) (rat-numer l2))))
    (make-rat n d)))

;; wypisywanie liczb wymiernych w formie czytelnej dla człowieka
(define (print-rat l)
  (display (rat-numer l))
  (display "/")
  (display (rat-denum l)))

;; cz. 2. Listy

;; predykat definiujący przyjętą przez nas reprezentację list
(define (list? x)
  (or (null? x)
      (and (pair? x)
           (list? (cdr x)))))

;; procedura obliczająca długość listy
(define (length xs)
  (if (null? xs)
      0
      (+ 1 (length (cdr xs)))))

;; procedura łącząca (konkatenująca) dwie listy
#;
(define (append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (append (cdr xs) ys))))


;; procedura accumulate (z ćwiczeń) jest skomplikowana: rozbijmy ją na mniejsze
(define (accumulate op null-val term start next end)
  (if (> start end)
      null-val
      (op (term start)
          (accumulate op null-val term (next start) next end))))


;; gen-sequence generuje ciąg liczb od start do end
;; procedura next determinuje kolejny element ciągu
(define (gen-sequence start next end)
  (if (> start end)
      null
      (cons start (gen-sequence (next start) next end))))

(define (square x) (* x x))

;; podnoszenie wszystkich elementów listy do kwadratu
(define (square-list xs)
  ;; zamiast poniższego wystarczy napisać (map square xs)!
  (if (null? xs)
      null
      (cons (square (car xs))
            (square-list (cdr xs)))))

;; i sumowanie wszystkich elementów listy
(define (sumlist xs)
  ;; zamiast poniższego wystarczy (fold_right + 0 xs)
  ;; możemy też powiedzieć (apply + xs), gdyż + jest procedurą która działa dla
  ;; dowolnej liczby argumentów. apply jest procedurą wbudowaną
  (if (null? xs)
      0
      (+ (car xs) (sumlist (cdr xs)))))

;; map aplikuje procedurę f do każdego z elementów listy i zwraca listę otrzymanych wyników
(define (map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (map f (cdr xs)))))

;; fold-right jest esencją accumulate z ćwiczeń: aplikuje operator op do
;; elementu listy i wyniku swojego wywołania na reszcie ciągu — a w przypadku gdy
;; ciąg się skończył zwraca nval
(define (fold-right op nval xs)
  (if (null? xs)
      nval
      (op (car xs)
          (fold-right op nval (cdr xs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;

;1
(define (make-rat2 n d)
  (let ((c (gcd n d)))
    (list (/ n c) (/ d c))))

(define (rat-num rat)
  (first rat))

(define (rat-den rat)
  (second rat))

(define (rat2? l)
(and (pair? l)
     (not (= (rat-den l) 0))
     (= 1 (gcd (rat-num l) (rat-den l)))))

(define (div-rat2 l1 l2)
  (let ((n (* (rat-num l1) (rat-den l2)))
        (d (* (rat-den l1) (rat-num l2))))
    (make-rat2 n d)))

(define (print-rat2 l)
  (display (rat-num l))
  (display "/")
  (display (rat-den l)))

(define rat0 (make-rat 2 1))
(define rat1 (make-rat2 1 1))
(define rat2 (make-rat2 1 1))
(define rat3 (make-rat2 2 1))
(define rat4 (make-rat2 2 1))

;2

(define (make-point x y)
  (list x y))

(define (point-x p)
  (first p))

(define (point-y p)
  (second p))

(define (point? p)
  (and (pair? p)
       (rat2? (point-x p))
       (rat2? (point-y p))))

(define p1 (make-point rat1 rat2))
(define p2 (make-point rat3 rat4))

(define (display-point p)
  (display "(")
  (print-rat2 (point-x p))
  (display ", ")
  (print-rat2 (point-y p))  
  (display ")"))

(define (make-vect p1 p2)
  (list p1 p2))

(define (vect-begin v)
  (first v))

(define (vect-end v)
  (second v))

(define (vect? v)
  (and (pair? v)
       (point? (vect-begin v))
       (point? (vect-end v))))

(define (vect-length v)
  (sqrt (+ (square (- (/ (rat-num(point-x(vect-end v)))
                         1.0
                         (rat-den(point-x(vect-end v))))
                      (/ (rat-num(point-x(vect-begin v)))
                         1.0
                         (rat-den(point-x(vect-begin v))))))
        (square (- (/ (rat-num(point-y(vect-end v)))
                      1.0
                      (rat-den(point-y(vect-end v))))
                   (/ (rat-num(point-y(vect-begin v)))
                      1.0
                      (rat-den(point-y(vect-begin v)))))))))
#;
(define (vect-scale v k)
  (make-vect (vect-begin v) (make-point)))

(define v1 (make-vect p1 p2))

(define (display-vect v)
  (display "[")
  (display-point (vect-begin v))
  (display ", ")
  (display-point (vect-end v))
  (display "]"))


;3
(define (make-vect2 p alfa d)
  (list p alfa d))

(define( 3-list? l)
  (and (list? l) (= 3 (length l))))

;selectory

#;(define (vect2? v)
  (and (3-list? v) (point? (vect-p v))))

#;(define (vect-length2 v)
  (vect-d v))

;4
(define (reverse xs)
  (if (null? xs)
      null
      (append (reverse (cdr xs))
              (list (car xs)))))

(define (reverse2 xs)
    (define (rev xs ys)
      (if (equal? null xs)
          ys
          (rev (cdr xs) (append (cons (car xs) null) ys))))
    (rev (cdr xs) (cons (car xs) null)))

(define l (list 1 2 3 4 5))
;(reverse2 l)

;5
(define (insert L n)
  (cond ((null? L)
         (list n))
        ((< n (car L))
         (cons n L))
        (else (cons (car L) (insert (cdr L) n)))))

#;
(define (insert-sort xs))

;6
(define (flatten-once x)
  (apply append x))

#;
(define (perm L n)
  (define (insert-at xs y n)
    (if (= n 0)
        (cons y xs)
        (cons (car xs) (insert-at (cdr xs) y (- n 1)))))
  (define (rek j lista)
    (cond ((null? lista) null)
          ((= j n) (rek 0 (cdr lista))
                   ))
  (if (null? L))))

;9
(define (append . L)
  (define (append-2 x y)
    (if(null? x)
       y
       (cons (car x) (append-2 (cdr x)
                               y))))
  (if (null? L)
     null
     (append-2 (car L) (apply append (cdr L)))))
;(apply append '( (1 (2 3)) ((4 5) 6)))