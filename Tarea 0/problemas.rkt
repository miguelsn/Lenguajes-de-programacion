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
(define (rectangle-properties chars)
  (list
    (*(list-ref chars 0) (list-ref chars 1))
    (+ (*(list-ref chars 0) 2) (*(list-ref chars 1) 2))
      )
  )

;; Problema 5

(define (find-needle* lst index)
  (if (null? lst) -1
      (if(eq? (first lst) 'needle)
         index
         (find-needle* (rest lst) (+ index 1)))))
  
(define (find-needle lst)
  (find-needle* lst 0))


;; Problema 6

(define (abs x)
  (if (negative? x) (* x -1) x))

;; Problema 7
(define (inclis1 ls)
  (map (lambda (x) (+ 1 x)) ls))
