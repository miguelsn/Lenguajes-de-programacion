#lang racket


; Problema 2
(define (bundle s n)
  (cond
    [(null? s) null]
    [(zero? n) null]
    [else (cons (implode (take s n))
                (bundle (drop s n) n))]))
; Problema 3
(define (take ls n)
  (if (or (null? ls) (zero? n))
      null
      (cons (first ls) (take (rest ls) (sub1 n)))))

(define (drop ls n)
  (if (or (null? ls) (zero? n))
      ls
      (drop (rest ls) (sub1 n))))


; Funciones auxiliares
(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (first x))
           (= (string-length (first x)) 1)
           (unit-string-list? (rest x)))))

(provide (all-defined-out))