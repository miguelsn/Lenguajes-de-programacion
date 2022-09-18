#lang plait

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPRESIONES DEL LENGUAJE EXTENDIDO ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type Value
  (numV [value : Number])
  (strV [value : String])
  (boolV [value : Boolean])
  (funV [param : Symbol] [body : ExprC]))

(define-type Operator
  (plusO)
  (appendO)
  (numeqO)
  (streqO))

(define-type ExprS
  (numS [value : Number])
  (strS [value : String])
  (boolS [value : Boolean])
  (idS [name : Symbol])
  (ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (andS [left : ExprS] [right : ExprS])
  (orS [left : ExprS] [right : ExprS])
  (binopS [op : Operator] [left : ExprS] [right : ExprS])
  (funS [param : Symbol] [body : ExprS])
  (letS [name : Symbol] [value : ExprS] [body : ExprS])
  (appS [func : ExprS] [arg : ExprS]))

(define-type ExprC
  (numC [value : Number])
  (strC [value : String])
  (boolC [value : Boolean])
  (idC [name : Symbol])
  (ifC [a : ExprC] [b : ExprC] [c : ExprC])
  (binopC [op : Operator] [left : ExprC] [right : ExprC])
  (funC [param : Symbol] [body : ExprC])
  (appC [func : ExprC] [arg : ExprC]))

(define-type Binding
  [bind (name : Symbol) (val : Value)])

(define-type-alias Env (Listof Binding))
(define empty-env empty)
(define extend-env cons)
(define default-env empty-env)
;;;;;;;;;;;;;;;
;; EVALUADOR ;;
;;;;;;;;;;;;;;;

(define (eval [str : S-Exp]) : Value
  (interp (desugar (parse str)) empty-env))


;;;;;;;;;;;;;
;; DESUGAR ;;
;;;;;;;;;;;;;

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [(numS value) (numC value)]
    [(strS value) (strC value)]
    [(boolS value) (boolC value)]
    [(idS name) (idC name)]
    [(ifS a b c) (ifC (desugar a) (desugar b) (desugar c))]
    [(andS left right) (ifC (desugar left) (desugar right) (boolC #f))]
    [(orS left right) (ifC (desugar left) (boolC #t) (desugar right))]
    [(binopS op left right)
     (binopC op (desugar left) (desugar right))]
    [(funS param body) (funC param (desugar body))]
    [(letS name value body) (appC (funC name (desugar body)) (desugar value))]
    [(appS func arg) (appC (desugar func) (desugar arg))]))


;;;;;;;;;;;;
;; INTERP ;;
;;;;;;;;;;;;

(define (interp [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
    [(numC value) (numV value)]
    [(strC value) (strV value)]
    [(boolC value) (boolV value)]
    [(idC name) (lookup-env name env)]
    [(ifC a b c)
     (let ([condition (interp a env)])
       (if (boolV? condition)
           (if (boolV-value condition)
               (interp b env)
               (interp c env))
           (error 'if "Argumento incorrecto")))]
    [(binopC op left right)
     (let ([left (interp left env)])
       (let ([right (interp right env)])
         (interp-binop op left right)))]
    [(funC param body) (funV param body)]
    [(appC func arg) 
     (let ([f (interp func env)])
       (if (funV? f)
           (let ([nenv (cons (bind (funV-param f) (interp arg env)) env)])
             (interp (funV-body f) nenv))
           (error 'interp "Call invalido" )))]))
(define (interp-binop [op : Operator]
                      [left : Value]
                      [right : Value]) : Value
  (type-case Operator op
    [(plusO)
     (if (numV? left)
         (if (numV? right)
             (numV (+ (numV-value left)
                      (numV-value right)))
             (error 'binop "Argumento incorrecto"))
         (error 'binop "Argumento incorrecto"))]
    [(appendO)
     (if (strV? left)
         (if (strV? right)
             (strV (string-append (strV-value left) (strV-value right)))
             (error 'binop "Argumento incorrecto"))
         (error 'binop "Argumento incorrecto"))]
    [(numeqO)
     (if (numV? left)
         (if (numV? right)
             (boolV (= (numV-value left) (numV-value right)))
             (error 'binop "Argumento incorrecto"))
         (error 'binop "Argumento incorrecto"))]
    [(streqO)
     (if (strV? left)
         (if (strV? right)
             (boolV (string=? (strV-value left) (strV-value right)))
             (error 'binop "Argumento incorrecto"))
         (error 'binop "Argumento incorrecto"))]))

(define (lookup-env [name : Symbol]
                    [env : Env]) : Value
  (cond
    [(empty? env) (error 'lookup-env "Unbound identifier")]
    [else (cond
            [(symbol=? name (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup-env name (rest env))])]))
;;;;;;;;;;;;
;; PARSER ;;
;;;;;;;;;;;;

(define (parse [in : S-Exp]) : ExprS
  (cond
    [(s-exp-number? in)                            (parse-number in)]
    [(s-exp-string? in)                            (parse-string in)]
    [(s-exp-match? `true in)                       (boolS #t)]
    [(s-exp-match? `false in)                      (boolS #f)]
    [(s-exp-match? `{if ANY ...} in)               (parse-if in)]
    [(s-exp-match? `{and ANY ...} in)              (parse-and in)]
    [(s-exp-match? `{or ANY ...} in)               (parse-or in)]
    [(s-exp-match? `{+ ANY ...} in)                (parse-+ in)]
    [(s-exp-match? `{++ ANY ...} in)               (parse-++ in)]
    [(s-exp-match? `{num= ANY ...} in)             (parse-num= in)]
    [(s-exp-match? `{str= ANY ...} in)             (parse-str= in)]
    [(s-exp-match? `{fun ANY ...} in)              (parse-fun in)]
    [(s-exp-match? `{let {SYMBOL ANY} ANY ...} in) (parse-let in)]
    [(s-exp-match? `{ANY ...} in)                  (parse-app in)]
    [(s-exp-symbol? in)                            (parse-id in)]))

(define (parse-number in)
  (numS (s-exp->number in)))

(define (parse-string in)
  (strS (s-exp->string in)))

(define (parse-id in)
  (idS (s-exp->symbol in)))

(define (parse-if in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 4)
        (ifS (parse (second inlst))
             (parse (third inlst))
             (parse (fourth inlst)))
        (error 'parse "cantidad incorrecta de argumentos para if"))))

(define (parse-and in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (andS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para and"))))

(define (parse-or in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (orS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para or"))))

(define (parse-+ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (plusO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-++ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (appendO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para ++"))))

(define (parse-num= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (numeqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para num="))))

(define (parse-str= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (streqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para str="))))

(define (parse-fun in)
  (cond
    [(s-exp-match? `{fun SYMBOL ANY ...} in)
     (let ([inlst (s-exp->list in)])
       (if (equal? (length inlst) 3)
           (funS (s-exp->symbol (second inlst)) (parse (third inlst)))
           (error 'parse "funciones deben tener solo un cuerpo")))]
    [(s-exp-match? `{fun ANY ...} in)
     (error 'parse "parametros a función deben ser símbolos")]))

(define (parse-let in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (letS
         (s-exp->symbol (first (s-exp->list (second inlst))))
         (parse (second (s-exp->list (second inlst))))
         (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para let"))))

(define (parse-app in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 2)
        (appS (parse (first inlst)) (parse (second inlst)))
        (error 'parse "cantidad incorrecta de argumentos en aplicación de funciones"))))
