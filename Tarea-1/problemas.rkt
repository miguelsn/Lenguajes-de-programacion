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
    (if(eqv? (first ls) x )
       (cons y (cons x (insertL 'x 'y (rest ls))))
       (cons (first ls) (insertL 'x 'y (rest ls))))))
  
; Problema 3

(define (remv-1st x ls)
  (if(eqv? ls '())
     null
     (if(eqv? (first ls) x )
        (rest ls)
        (cons (first ls) (remv-1st x (rest ls))))))

; Problema 4
(define (map proc ls)
  (if(null? ls)
     null
     (cons (proc (first ls)) (map proc (rest ls)))))

; Problema 5
(define (filter proc ls)
  (if (null? ls)
      null
      (if (proc (first ls))
          (cons (first ls) (filter proc (rest ls)))
          (filter proc (rest ls)))))

; Problema 6
(define (zip ls lz)
  (if (or (null? ls) (null? lz))
      null
      (cons (cons (first ls) (first lz)) (zip (rest ls) (rest lz))))) 

; Problema 7
(define (list-index-ofv* x ls index)
  (if (null? ls)
      -1
      (if (eq? (first ls) x)
          index
          (list-index-ofv* x (rest ls) (+ index 1)))))

(define (list-index-ofv x ls)
  (list-index-ofv* x ls 0))

; Problema 8
(define (append ls lz)
  (if (and (null? ls) (null? lz))
      null
      (if (null? ls)
          (append lz ls)
          (cons (first ls) (append (rest ls) lz)))))       

; Problema 9 
(define (reverse ls)
  (if (null? ls)
      null
      (cons (last(ls)) reverse(ls))))

; Problema 10
(define (repeat ls n)
  (if(eq? n 1)
     ls
     (append ls (repeat ls (sub1 n)))))



(provide (all-defined-out))