#lang plait

(define-type ArithC
  [numC (num : Number)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

(define-type ArithS
  [numS (num : Number)]
  [plusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [subS (l : ArithS) (r : ArithS)]
  [negS (e : ArithS)])

(define (parse [s : S-Exp]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([ls (s-exp->list s)])
       (if (not (empty? ls))
           (case (s-exp->symbol (first ls))
             [(+)
              (if (eq? (length ls) 3)
                  (plusS (parse (second ls)) (parse (third ls)))
                  (error 'parse "operacion aritmetica mal formada"))]
             [(*)
              (if (eq? (length ls) 3)
                  (multS (parse (second ls)) (parse (third ls)))
                  (error 'parse "operacion aritmetica mal formada"))]
             
             [(-)
              (if (eq? (length ls) 3)
                  (subS (parse (second ls)) (parse (third ls)))
                  (error 'parse "operacion aritmetica mal formada"))]
             
             [(n)
              (if (eq? (length ls) 2)
                  (negS (parse (second ls)))
                  (error 'parse "operacion aritmetica mal formada"))]

             [else (error 'parse "operacion aritmetica mal formada")])
           (error 'parse "operacion aritmetica mal formada")))]
    [else (error 'parse "operacion aritmetica mal formada")]))

(define (desugar [s : ArithS]) : ArithC
  (type-case ArithS s
    [(numS n) (numC n)]
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(multS l r) (multC (desugar l) (desugar r))]
    [(subS l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [(negS x) (multC (numC -1) (desugar x))]))

(define (interp [a : ArithC]) : Number
  (type-case ArithC a
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]))

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))