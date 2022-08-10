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
(define (find-needle lst)
  (index-of lst 'needle))

;; Problema 6
