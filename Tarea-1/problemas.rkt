#lang racket
(require racket/trace)

; Problema 1
(define (countdown n)
  (if(zero? n)
    (cons n '())
    (cons n (countdown (sub1 n)))))


  

    
