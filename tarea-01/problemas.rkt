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
      (cons (last ls) (reverse (drop-right ls 1)))))

; Problema 10
(define (repeat ls n)
  (if(eq? n 1)
     ls
     (append ls (repeat ls (sub1 n)))))

; Problema 11
(define (same-lists* ls lz)
  (if (and (null? ls) (null? lz))
      #t
      (if (or (null? ls) (null? lz))
          #f
          (if (equal? (first ls) (first lz))
              (same-lists* (rest ls) (rest lz))
              #f))))
; Problema 12
(define (eqv)
  (equal? '((w . (x . ())) y (z . ())) '((w x) y (z))))

; Problema 13
(define (binary->natural ls)
  (binary->natural* ls 0))

(define (binary->natural* ls n)
  (if (null? ls)
      0
      (+ (* (first ls) (expt 2 n)) (binary->natural* (rest ls) (add1 n)))))

; Problema 14
(define (div x y)
  (div* x y 1))

(define (div* x y n)
  (if (> (* y n) x)
     #f
     (if(eq? (* y n) x)
        n
     (div* x y (add1 n)))))
(provide (all-defined-out))

; Problema 15
(define (append-map proc1 proc2)
  (if(null? proc2)
     null
     (append (proc1 (first proc2)) (append-map proc1 (rest proc2)))))

; Problema 16
(define (set-difference ls lz)
  (if (null? ls)
      null
      (if (eq? (member (first ls) lz) #f)
          (cons (first ls) (set-difference (rest ls) lz))
          (set-difference (rest ls) lz))))

; Problema 17
(define (foldr op x ls)
  (if (null? ls)
      x
      (op (first ls) (foldr op x (rest ls)))))

; Problema 18
(define (powerset ls)
  (if (null? ls)
      (list ls)
       (let ([ps (powerset (rest ls))])
             (append (ch-append (first ls) ps) ps))))

; Problema 19
(define (cartesian ls lz)
  (if(null? ls)
     null
     (if (null? lz)
         ls
         (append (map (lambda (x)
                        (if (list? (first ls))
                            (append (first ls) (cons x null))
                            (append (list (first ls)) (cons x null)))) lz) (cartesian (rest ls) lz)))))

(define (cartesian-product ls)
 (if(eq? (length ls) 1)
     ls
     (cartesian-product (cons (cartesian (first ls) (second ls)) (rest (rest ls))))))

; Problema 20
; insertL
(define (insertL-fr x y ls)
(foldr (lambda (z zs)
         (if (eqv? z x)
             (cons y (cons x zs))
             (cons z zs)))
       '()
       ls))

; filter
(define (filter-fr proc ls)
  (foldr (lambda (x xs)
           (if (proc x)
               (cons x xs)
               xs))
         '()
         ls))
  
; map
(define (map-fr op ls)
  (foldr (lambda (x xs)
           (cons (op x) xs))
         '()
         ls))
; append
(define (append-fr ls lz)
  (if (null? ls)
      (foldr (lambda (x xs)
               (if (null? xs)
                   (cons x ls)
                       (cons x xs)))     
             '()
             lz)
      (foldr (lambda (x xs)
               (if (null? xs)
                   (cons x lz)
                       (cons x xs)))     
             '()
             ls)))
; reverse
(define (reverse-fr ls)
  (foldr (lambda (x xs)
           (append xs (list x)))
         '()
         ls))

; binary-> natural
(define (binary->natural-fr ls)
  (foldr (lambda (x y)
           (+ x (* 2 y))) 0 ls))

      
; append-map
(define (append-map-fr proc1 proc2)
  (foldr (lambda (x xs)
           (append (proc1 x) xs))
         '()
         proc2))
 
;set-difference-fr
(define (set-difference-fr ls lz)
  (foldr (lambda (x xs)
           (if (eq? (member x lz) #f)
               (cons x xs)
               xs))
         '()
         ls))
;powerset-fr
(define (powerset-fr ls)
  (foldr (lambda (x xs)
            (append-map (lambda (y)
                          (list (cons x y) y))
                        xs))
         '(())
         ls))
; Problema 21
(define snowball
  (letrec
      ((odd-case
        (lambda (fix-odd)
          (lambda (x)
            (cond
             ((and (exact-integer? x) (positive? x) (odd? x))
               (snowball (add1 (* x 3))))
              (else (fix-odd x))))))
       (even-case
        (lambda (fix-even)
          (lambda (x)
            (cond
              ((and (exact-integer? x) (positive? x) (even? x))
               (snowball (/ x 2)))
              (else (fix-even x))))))
       (one-case
        (lambda (fix-one)
          (lambda (x)
            (cond
              ((zero? (sub1 x)) 1)
              (else (fix-one x))))))
       (base
        (lambda (x)
          (error "a"))))
    
    (one-case (even-case (odd-case base)))))



; Funciones auxiliares
(define (ch-append x ls)
	(if (null? ls) 
		null
		(cons (cons x (first ls)) (ch-append x (rest ls)))))

(provide (all-defined-out))