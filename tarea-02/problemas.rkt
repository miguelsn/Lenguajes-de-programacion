#lang racket
(require pict
         pict/color)

; Problema 2, 17

; bundle : (listof symbol?) integer? -> list
(define (bundle s n)
  (cond
    [(> n 0)
     (cond
    [(null? s) null]
    [else (cons (implode (take s n))
                (bundle (drop s n) n))])]
    [else (error 'bundle "Argumentos invalidos" s)]))

; Problema 3

; take : list?, integer? -> list
(define (take ls n)
  (if (or (null? ls) (zero? n))
      null
      (cons (first ls) (take (rest ls) (sub1 n)))))

; drop : list?, integer? -> list
(define (drop ls n)
  (if (or (null? ls) (zero? n))
      ls
      (drop (rest ls) (sub1 n))))


; Problema 6
; list->chunks : list?, integer? -> list
(define (list->chunks ls n)
  (cond
    [(or (null? ls) (zero? n)) null]
    [else (cons (take ls n) (list->chunks (drop ls n) n))]))

; bundle-chunks : list?, integer? -> list
(define (bundle-chunks ls n)
  (let loop ([lz (list->chunks ls n)])
    (if (null? lz)
        null
        (cons (implode (first lz)) (loop (rest lz))))))

; Problema 7

; partition : string?, integer? -> list
(define (partition s n)
  (if (> n 0)
      (if (> n (string-length s))
          (if (eq? (string-length s) 0)
              null
              (list s))
          (cons (substring s 0 n) (partition (substring s n) n)))
      null))


; Problema 8
; isort : list? -> list
(define (isort ls)
  (if(empty? ls)
     null
     (insert (first ls)(isort (rest ls)))))

; insert : integer?, list? -> list
(define (insert n ls)
  (cond
    [(empty? ls) (list n)]
    [(<= n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls)))]))

; Problema 10
; smallers : list?, integer? -> list
(define (smallers ls n)
  (if (empty? ls)
      null
      (if (or (> (first ls) n) (eq? (first ls) n))
          (smallers (rest ls) n)
          (cons (first ls) (smallers (rest ls) n)))))

; largers : list?, integer? -> list
(define (largers ls n)
  (if (empty? ls)
      null
      (if (or (> n (first ls)) (eq? (first ls) n))
          (largers (rest ls) n)
          (cons (first ls) (largers (rest ls) n)))))

; Problema 11

; quicksort : list? -> list
(define (quicksort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
       (append (quicksort (smallers ls pivot))
               (equals ls pivot)
               (quicksort (largers ls pivot)))]))

; equals : list?, integer? -> list
(define (equals ls n)
  (if (null? ls)
      null
      (if (eq? (first ls) n)
          (cons n (equals (rest ls) n))
          (equals (rest ls) n))))

; Problema 12
(define (general-quicksort ls proc)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
       (append (general-quicksort (filter (lambda (x) (proc x pivot)) ls) proc)
               (filter (lambda (x) (equal? x pivot)) ls)
               (general-quicksort (filter (lambda (x) (and (not (proc x pivot)) (not (equal? x pivot)))) ls) proc))]))
; Problema 13
(define (iquicksort ls)
  (cond
    [(empty? ls) null]
    [(<= (length ls) 150) (isort ls)]
    [else
     (define pivot (first ls))
       (append (quicksort (smallers ls pivot))
               (equals ls pivot)
               (quicksort (largers ls pivot)))]))

; Problema 14
; smallers-filter : list?, integer? -> list
(define (smallers-filter ls n)
  (filter (lambda (x)
            (> n x)) ls))
; largers-filter : list?, integer? -> list
(define (largers-filter ls n)
  (filter (lambda (x)
            (> x n)) ls))

; Problema 15
; qsort : list? -> list
(define (qsort ls)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
       (append (quicksort (let smaller ([lz ls] [n pivot])
                            (if (empty? lz)
                                null
                                (if (or (> (first lz) n) (eq? (first lz) n))
                                    (smaller (rest lz) n)
                                    (cons (first lz) (smaller (rest lz) n))))))
               (equals ls pivot)
               (quicksort (let larger ([lz ls] [n pivot])
                            (if (empty? lz)
                                null
                                (if (or (< (first lz) n) (eq? (first lz) n))
                                    (larger (rest lz) n)
                                    (cons (first lz) (larger (rest lz) n)))))))]))

  
; Fractal

(define (circles n s)
  (let ([p (white (circle s))])
    (if (eq? n 1)
        (hc-append p p)
        (hc-append (cc-superimpose p (circles (- n 1) (/ s 2))) (cc-superimpose p (circles (- n 1) (/ s 2)))))))

(define (fractal)
  (circles 10 500))

; Funciones auxiliares
; explode : string? -> list
(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

; implode : list? -> string
(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e" ls))
  (apply string-append ls))

; unit-string-list? : list? -> boolean
(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (first x))
           (= (string-length (first x)) 1)
           (unit-string-list? (rest x)))))

; random-list : integer? -> list
(define (random-list n)
  (if (eq? n 0)
      null
      (cons (random 100) (random-list (sub1 n)))))

(define (gdc-structural n m)
  (define (find-largest-divisor k)
    (cond [(= k 1) 1]
          [(= (remainder n k) (remainder m k) 0) k]
          [else (find-largest-divisor (- k 1))]))
  (find-largest-divisor (min n m)))

(define (gdc-generative n m)
  (define (find-largest-divisor max min)
    (if (= min 0)
        max
        (find-largest-divisor min (remainder max min))))
  (find-largest-divisor (max n m) (min n m)))

(define (timer)
  (let ([x 107000] [y 170000])
        (time (gdc-structural x y))
        (time (gdc-generative x y)))
  (void))
(provide (all-defined-out))