#lang racket

;; Problema 1
(define pi 3.14)

;; Problema 2
(define (area-circle r)
  (* pi r r))

;; Problema 3
(define (circle-properties r)
  (list (* pi r r) (* pi r 2)))

;; Problema 4
(define (rectangle-properties rec)
  (list
    (*(list-ref rec 0) (list-ref rec 1))
    (+ (*(list-ref rec 0) 2) (*(list-ref rec 1) 2))))

;; Problema 5

(define (find-needle* ls index)
  (if (null? ls) 
      -1
      (if (eq? (first ls) 'needle)
         index
         (find-needle* (rest ls) (+ index 1)))))
  
(define (find-needle ls)
  (find-needle* ls 0))


;; Problema 6

(define (abs x)
  (if (negative? x) (* x -1) x))

;; Problema 7
(define (inclis1 ls)
  (map (lambda (x) (+ 1 x)) ls))

;; Problema 8
(define (even? x)
  (if (integer? (/ x 2)) #t #f))

;; Problema 9
(define another-add
  (lambda (n m)
    (cond
         [(zero? n) m]
         [else (add1 (another-add (sub1 n) m ))])))

(provide (all-defined-out))