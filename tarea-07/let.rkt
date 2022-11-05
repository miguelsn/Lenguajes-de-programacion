#lang racket

#|

 /**       /******** /********
| **      | **_____/|__  **__/
| **      | **         | **   
| **      | *****      | **   
| **      | **__/      | **   
| **      | **         | **   
| ********| ********   | **   
|________/|________/   |__/   

SINTAXIS CONCRETA
=================

Program := Expression
Expression := Number
Expression := -(Expression , Expression)
Expression := zero? (Expression)
Expression := if Expression then Expression else Expression
Expression := Identifier
Expression := let Identifier = Expression in Expression

SINTAXIS CONCRETA (S-Expressions)
=================================

Program := Expression
Expression := Number
Expression := (- Expression Expression)
Expression := (zero? Expression)
Expression := (if Expression Expression Expression)
Expression := Identifier
Expression := (let (Identifier Expression) Expression)

SINTAXIS ABSTRACTA
==================

Programa:
- (a-program exp1)

Expresión:
- (const-exp num)
- (diff-exp exp1 exp2)
- (zero?-exp exp1)
- (if-exp exp1 exp2 exp3)
- (var-exp var)
- (let-exp var exp1 body)

|#

(struct a-program (exp1)
  #:transparent)

(define (program? x)
  (a-program? x))

(struct const-exp (num)
  #:transparent)

(struct diff-exp (exp1 exp2)
  #:transparent)

(struct zero?-exp (exp1)
  #:transparent)

(struct if-exp (exp1 exp2 exp3)
  #:transparent)

(struct var-exp (var)
  #:transparent)

(struct let-exp (var exp1 body)
  #:transparent)

(define (expression? x)
  (or (const-exp? x)
      (diff-exp? x)
      (zero?-exp? x)
      (if-exp? x)
      (var-exp? x)
      (let-exp? x)))

;; parse : S-Expression -> program?
;; Traducción de S-Expressions a sintaxis abstracta de LET
(define (parse x)
  (a-program (parse-expression x)))

;; parse-expression : S-Expression -> expression?
(define (parse-expression x)
  (cond
    [(number? x) (const-exp x)]
    [(symbol? x) (var-exp x)]
    [(pair? x)
     (case (first x)
       [(-) (parse-diff x)]
       [(zero?) (parse-zero? x)]
       [(if) (parse-if x)]
       [(let) (parse-let x)]
       [else
        (error 'parse "expresión no es válida: ~e" x)])]
    [else
     (error 'parse "expresión no es válida: ~e" x)]))

;; parse-diff : pair? -> diff-exp?
;; x es un par de la forma (- . _)
(define (parse-diff x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (diff-exp (parse-expression (second x))
            (parse-expression (third x))))

;; parse-zero? : pair? -> zero?-exp?
;; x es un par de la forma (zero? . _)
(define (parse-zero? x)
  (unless (= (length x) 2)
    (error 'parse "expresión no es válida: ~e" x))
  (zero?-exp (parse-expression (second x))))

;; parse-if : pair? -> if-exp?
;; x es un par de la forma (if . _)
(define (parse-if x)
  (unless (= (length x) 4)
    (error 'parse "expresión no es válida: ~e" x))
  (if-exp (parse-expression (second x))
          (parse-expression (third x))
          (parse-expression (fourth x))))

;; parse-let : pair? -> let-exp?
;; x es un par de la forma (let . _)
(define (parse-let x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (let ([binding (second x)])
    (unless (= (length binding) 2)
      (error 'parse "expresión no es válida: ~e" x))
    (unless (symbol? (first binding))
      (error 'parse "expresión no es válida: ~e" x))
    (let-exp (first binding)
             (parse-expression (second binding))
             (parse-expression (third x)))))

;;;;;;;;;;;;;;
;; ENTORNOS ;;
;;;;;;;;;;;;;;

(define (empty-env)
  null)

(define (apply-env env var)
  (if (null? env)
      (error 'environment "variable libre: ~e" var)
      (let ([binding (first env)])
        (if (equal? var (first binding))
            (second binding)
            (apply-env (rest env) var)))))

(define (extend-env var val env)
  (cons (list var val) env))

;;;;;;;;;;;;;;;
;; SEMÁNTICA ;;
;;;;;;;;;;;;;;;

#|

VALORES EXPRESADOS Y DENOTADOS

ExpVal = Int + Bool
DenVal = Int + Bool

|#

(struct num-val (num)
  #:transparent
  #:guard (lambda (num type-name)
            (unless (number? num)
              (error type-name "no es un número: ~e" num))
            num))

(define expval->num num-val-num)

(struct bool-val (bool)
  #:transparent
  #:guard (lambda (bool type-name)
            (unless (boolean? bool)
              (error type-name "no es un booleano: ~e" bool))
            bool))

(define expval->bool bool-val-bool)

#|

ESPECIFICACIONES SEMÁNTICAS

(value-of (const-exp n) env) = (num-val n)

(value-of (var-exp var) env) = (apply-env env var)

(value-of (diff-exp exp1 exp2) env)
 = (num-val
    (- (expval->num (value-of exp1 env))
       (expval->num (value-of exp2 env))))

(value-of (zero?-exp exp1) env)
 = (if (equal? 0 (expval->num (value-of exp1 env)))
       (bool-val #t)
       (bool-val #f))

(value-of (if-exp exp1 exp2 exp3) env)
 = (if (expval->bool (value-of exp1 env))
       (value-of exp2 env)
       (value-of exp3 env))

(value-of (let-exp var exp1 body) env)
 = (value-of body (extend-env var (value-of exp1 env) env))

|#

(define (value-of exp env)
  (cond
    [(const-exp? exp)
     (let ([n (const-exp-num exp)])
       (num-val n))]
    [(var-exp? exp)
     (let ([var (var-exp-var exp)])
       (apply-env env var))]
    [(diff-exp? exp)
     (let ([exp1 (diff-exp-exp1 exp)]
           [exp2 (diff-exp-exp2 exp)])
       (num-val
        (- (expval->num (value-of exp1 env))
           (expval->num (value-of exp2 env)))))]
    [(zero?-exp? exp)
     (let ([exp1 (zero?-exp-exp1 exp)])
       (if (equal? 0 (expval->num (value-of exp1 env)))
           (bool-val #t)
           (bool-val #f)))]
    [(if-exp? exp)
     (let ([exp1 (if-exp-exp1 exp)]
           [exp2 (if-exp-exp2 exp)]
           [exp3 (if-exp-exp3 exp)])
       (if (expval->bool (value-of exp1 env))
           (value-of exp2 env)
           (value-of exp3 env)))]
    [(let-exp? exp)
     (let ([var (let-exp-var exp)]
           [exp1 (let-exp-exp1 exp)]
           [body (let-exp-body exp)])
       (value-of body (extend-env var (value-of exp1 env) env)))]
    [else
     (error 'value-of "no es una expresión: ~e" exp)]))

(define (init-env)
  (foldl (lambda (binding env)
           (extend-env (first binding) (second binding) env))
         (empty-env)
         (list (list 'π (num-val 3.141592653589793))
               (list 'e (num-val 2.718281828459045))
               (list 'i (num-val 0+1i))
               (list 'G (num-val 6.674e-11))
               (list 'c (num-val 299792458))
               (list 'h (num-val 6.62607015e-34)))))

(define (value-of-program pgm)
  (if (program? pgm)
      (let ([exp1 (a-program-exp1 pgm)])
        (value-of exp1 (init-env)))
      (error 'value-of-program "no es un programa: ~e" pgm)))

(define (run sexp)
  (value-of-program (parse sexp)))
