#lang racket

(define (mreverse mlist)
  
  (define (mlength mlist)
    (define (mlen mlist acc)
      (if (null? (mcdr mlist))
          (+ acc 1)
          (mlen (mcdr mlist) (+ acc 1))))
    (if (null? mlist)
        0
        (mlen mlist 0)))
  
  (define (go-n-get mlist length)
    (if (> length 0)
        (go-n-get (mcdr mlist) (- length 1))
        mlist))
  
  (define (mrev mlist length)
    (if (> length 0)
        (let* ((switch (go-n-get mlist length))
               (tmp (mcar switch)))
          (and (set-mcar! switch (mcar mlist))
               (set-mcar! mlist tmp)
               (mrev (mcdr mlist) (- length 2))))
        (display "")))
  
  (mrev mlist (- (mlength mlist) 1)))

(define (mreverse2 mlist)
  (define (mrev mlist acc)
    (if (null? (mcdr mlist))
        (and (set-mcdr! mlist acc)
             mlist)
        (let ((tmp (mcdr mlist)))
          (and (set-mcdr! mlist acc)
               (mrev tmp mlist)))))
  (mrev mlist null))

(define list
  (mcons 1 (mcons 2 (mcons 3 (mcons 4 null)))))

(define list2
  (mcons 1 (mcons 2 (mcons 3 (mcons 4 null)))))

list

(mreverse list)

list

(displayln "")

list2

(mreverse2 list2)
  

