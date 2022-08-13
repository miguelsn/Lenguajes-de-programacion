#lang racket
(require racket/trace)

; Problema 1
(define (countdown n)
  (if(zero? n)
    (cons n '())
    (cons n (countdown (sub1 n)))))

; Problema 2

(define (insertL x y ls)
 (if(eqv? ls '())
    null
    (if(eqv? (first ls) 'x )
       (cons y (cons x (insertL 'x 'y (rest ls))))
       (cons (first ls) (insertL 'x 'y (rest ls))))))
  

    
