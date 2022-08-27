#lang racket


; Problema 2
(define (bundle s n)
  (cond
    [(null? s) null]
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

; Problema 6
(define (list->chunks ls n)
  (cond
    [(null? ls) null]
    [else (cons (take ls n) (list->chunks (drop ls n) n))]))

(define (bundle-chunks ls n)
  (let loop ([lz (list->chunks ls n)])
    (if (null? lz)
        null
        (cons (implode (first lz)) (loop (rest lz))))))

; Problema 7
(define (partition s n)
  (if (> n (string-length s))
      (if (eq? (string-length s) 0)
      null
      (list s))
      (cons (substring s 0 n) (partition (substring s n) n))))
        
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